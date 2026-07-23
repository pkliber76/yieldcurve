#' Year Fraction
#'
#' Computes year fractions between two dates using a simple day-count
#' convention. The default is ACT/365.25, matching the package's current
#' preliminary time-to-maturity convention.
#'
#' @param start_date A `Date` vector of start dates.
#' @param end_date A `Date` vector of end dates.
#' @param day_count Day-count convention. Currently `"ACT/365.25"` and
#'   `"ACT/365"` are supported.
#'
#' @return A numeric vector of year fractions.
#' @export
#'
#' @examples
#' year_fraction(as.Date("2026-01-01"), as.Date("2027-01-01"))
year_fraction <- function(start_date, end_date, day_count = "ACT/365.25") {
  if (!inherits(start_date, "Date")) {
    stop("start_date must be a Date vector.", call. = FALSE)
  }
  if (!inherits(end_date, "Date")) {
    stop("end_date must be a Date vector.", call. = FALSE)
  }

  day_count <- match.arg(day_count, c("ACT/365.25", "ACT/365"))
  denominator <- switch(
    day_count,
    "ACT/365.25" = 365.25,
    "ACT/365" = 365
  )

  as.numeric(end_date - start_date) / denominator
}

#' Calculate Dirty Prices from Clean Prices
#'
#' Calculates dirty prices from quoted clean prices by adding accrued interest.
#' Accrued interest is calculated from each bond's coupon schedule using the
#' issue date, maturity date, coupon rate, and coupon frequency.
#'
#' @param bonds A data frame with columns `bond_id`, `issue_date`, `maturity`,
#'   `coupon_rate`, and `coupon_frequency`.
#' @param quotes A wide clean-price data frame. The first column must be
#'   `date`; each remaining column is one bond. Missing values mean that the
#'   bond was not quoted on that date.
#' @param face_value Numeric face value. Use a single value for all bonds or one
#'   value per row of `bonds`. Defaults to `100`.
#' @param day_count Day-count convention for accrued interest. Currently
#'   `"ACT/ACT"`, `"ACT/365.25"`, and `"ACT/365"` are supported. `"ACT/ACT"`
#'   uses the actual number of days in the coupon period.
#'
#' @return A wide dirty-price data frame with the same date rows and bond
#'   columns as the clean-price panel.
#' @export
#'
#' @examples
#' bonds <- data.frame(
#'   bond_id = c("B1", "B2"),
#'   issue_date = as.Date(c("2026-01-01", "2026-01-01")),
#'   maturity = as.Date(c("2028-01-01", "2031-01-01")),
#'   coupon_rate = c(0.04, 0.055),
#'   coupon_frequency = c(2L, 2L)
#' )
#'
#' quotes <- data.frame(
#'   date = as.Date("2026-07-01"),
#'   B1 = 100.25,
#'   B2 = 98.75
#' )
#'
#' calculate_dirty_prices(
#'   bonds = bonds,
#'   quotes = quotes,
#'   face_value = c(100, 100)
#' )
calculate_dirty_prices <- function(bonds,
                                   quotes,
                                   face_value = 100,
                                   day_count = "ACT/ACT") {
  clean_panel <- clean_price_panel_from_quotes(quotes, bonds)
  accrued_panel <- calculate_accrued_interest(
    bonds = bonds,
    prices = clean_panel,
    face_value = face_value,
    day_count = day_count
  )

  dirty_panel <- clean_panel
  bond_columns <- setdiff(names(dirty_panel), "date")
  for (id in bond_columns) {
    dirty_panel[[id]] <- clean_panel[[id]] + accrued_panel[[id]]
  }

  dirty_panel
}

#' Calculate Accrued Interest Panel
#'
#' Calculates accrued interest for every date and bond in a clean-price panel.
#' Values are returned in the same wide layout as prices. Cells outside a bond's
#' life are `NA`.
#'
#' @param bonds A data frame with columns `bond_id`, `issue_date`, `maturity`,
#'   `coupon_rate`, and `coupon_frequency`.
#' @param prices A wide price panel with first column `date`. The function uses
#'   the dates and bond columns from this panel.
#' @param face_value Numeric face value. Use a single value for all bonds or one
#'   value per row of `bonds`. Defaults to `100`.
#' @param day_count Day-count convention for accrued interest. Currently
#'   `"ACT/ACT"`, `"ACT/365.25"`, and `"ACT/365"` are supported.
#'
#' @return A wide data frame with first column `date` and accrued-interest
#'   columns for each bond.
#' @export
calculate_accrued_interest <- function(bonds,
                                       prices,
                                       face_value = 100,
                                       day_count = "ACT/ACT") {
  validate_bonds(bonds)
  validate_price_panel(prices, bonds = bonds)
  day_count <- match.arg(day_count, c("ACT/ACT", "ACT/365.25", "ACT/365"))

  face_value <- normalize_bond_vector(face_value, bonds$bond_id, "face_value", positive = TRUE)
  names(face_value) <- bonds$bond_id

  bond_columns <- setdiff(names(prices), "date")
  out <- data.frame(date = prices$date, stringsAsFactors = FALSE)

  for (id in bond_columns) {
    bond <- bonds[match(id, bonds$bond_id), , drop = FALSE]
    values <- accrued_interest_for_dates(
      bond = bond,
      settlement_date = prices$date,
      face_value = unname(face_value[id]),
      day_count = day_count
    )
    out[[id]] <- values
  }

  out
}

clean_price_panel_from_quotes <- function(quotes, bonds = NULL) {
  prepare_clean_prices(quotes, bonds = bonds, keep_empty_dates = TRUE)
}

price_long_to_wide <- function(data, value_column, dates, bond_id) {
  out <- data.frame(date = as.Date(dates), stringsAsFactors = FALSE)
  for (id in bond_id) {
    out[[id]] <- NA_real_
  }

  if (nrow(data) == 0) {
    return(out)
  }

  for (i in seq_len(nrow(data))) {
    id <- data$bond_id[i]
    if (id %in% bond_id) {
      out[match(data$date[i], out$date), id] <- data[[value_column]][i]
    }
  }
  out
}

dirty_price_panel_from_input <- function(dirty_prices, bonds = NULL) {
  validate_price_panel(dirty_prices, bonds = bonds, price_type = "dirty")
  dirty_prices[order(dirty_prices$date), , drop = FALSE]
}

calculate_dirty_price_data_frame <- function(bonds,
                                             quotes,
                                             face_value = 100,
                                             day_count = "ACT/ACT") {
  validate_inputs(bonds, quotes)
  face_value <- normalize_bond_vector(face_value, bonds$bond_id, "face_value", positive = TRUE)
  names(face_value) <- bonds$bond_id

  day_count <- match.arg(day_count, c("ACT/ACT", "ACT/365.25", "ACT/365"))
  clean_panel <- clean_price_panel_from_quotes(quotes, bonds)
  dirty_panel <- calculate_dirty_prices(
    bonds = bonds,
    quotes = clean_panel,
    face_value = face_value,
    day_count = day_count
  )
  accrued_panel <- calculate_accrued_interest(
    bonds = bonds,
    prices = clean_panel,
    face_value = face_value,
    day_count = day_count
  )

  clean_data <- as_quote_data_frame(clean_panel)
  dirty_data <- price_panel_to_long(dirty_panel, "dirty_price")
  accrued_data <- price_panel_to_long(accrued_panel, "accrued_interest")

  if (nrow(clean_data) == 0) {
    out <- merge(
      clean_data,
      bonds,
      by = "bond_id",
      all.x = TRUE,
      all.y = FALSE,
      sort = FALSE
    )
    out$time_to_maturity <- numeric()
    out$face_value <- numeric()
    out$accrued_interest <- numeric()
    out$dirty_price <- numeric()
    return(out)
  }

  prepared <- merge(clean_data, dirty_data, by = c("date", "bond_id"), sort = FALSE)
  prepared <- merge(prepared, accrued_data, by = c("date", "bond_id"), sort = FALSE)
  prepared <- merge(
    prepared,
    bonds,
    by = "bond_id",
    all.x = TRUE,
    all.y = FALSE,
    sort = FALSE
  )
  prepared$time_to_maturity <- as.numeric(prepared$maturity - prepared$date) / 365.25
  prepared <- prepared[prepared$time_to_maturity > 0, , drop = FALSE]
  prepared$face_value <- unname(face_value[prepared$bond_id])
  rownames(prepared) <- NULL
  prepared
}

#' Calculate Clean Prices from Dirty Prices
#'
#' Calculates clean prices from quoted dirty prices by subtracting accrued
#' interest. Accrued interest is calculated from each bond's coupon schedule
#' using the issue date, maturity date, coupon rate, and coupon frequency.
#'
#' @param bonds A data frame with columns `bond_id`, `issue_date`, `maturity`,
#'   `coupon_rate`, and `coupon_frequency`.
#' @param dirty_prices A wide dirty-price data frame. The first column must be
#'   `date`; each remaining column is one bond. Missing values mean that the
#'   bond was not quoted on that date.
#' @param face_value Numeric face value. Use a single value for all bonds or one
#'   value per row of `bonds`. Defaults to `100`.
#' @param day_count Day-count convention for accrued interest. Currently
#'   `"ACT/ACT"`, `"ACT/365.25"`, and `"ACT/365"` are supported. `"ACT/ACT"`
#'   uses the actual number of days in the coupon period.
#'
#' @return A wide clean-price data frame with the same date rows and bond
#'   columns as the dirty-price panel.
#' @export
#'
#' @examples
#' bonds <- data.frame(
#'   bond_id = "B1",
#'   issue_date = as.Date("2026-01-01"),
#'   maturity = as.Date("2027-01-01"),
#'   coupon_rate = 0.04,
#'   coupon_frequency = 2L
#' )
#' dirty_prices <- data.frame(
#'   date = as.Date("2026-04-01"),
#'   B1 = 100
#' )
#' calculate_clean_prices(bonds, dirty_prices)
calculate_clean_prices <- function(bonds,
                                   dirty_prices,
                                   face_value = 100,
                                   day_count = "ACT/ACT") {
  dirty_panel <- dirty_price_panel_from_input(dirty_prices, bonds)
  accrued_panel <- calculate_accrued_interest(
    bonds = bonds,
    prices = dirty_panel,
    face_value = face_value,
    day_count = day_count
  )

  clean_panel <- dirty_panel
  bond_columns <- setdiff(names(clean_panel), "date")
  for (id in bond_columns) {
    clean_panel[[id]] <- dirty_panel[[id]] - accrued_panel[[id]]
  }

  observed <- !is.na(as.matrix(clean_panel[, bond_columns, drop = FALSE]))
  if (any(!is.finite(as.matrix(clean_panel[, bond_columns, drop = FALSE]))[observed]) ||
      any(as.matrix(clean_panel[, bond_columns, drop = FALSE])[observed] <= 0)) {
    stop("Calculated clean prices must be positive and finite.", call. = FALSE)
  }

  clean_panel
}

calculate_clean_price_data_frame <- function(bonds,
                                             dirty_prices,
                                             face_value = 100,
                                             day_count = "ACT/ACT") {
  validate_bonds(bonds)
  validate_dirty_prices(dirty_prices)

  face_value <- normalize_bond_vector(face_value, bonds$bond_id, "face_value", positive = TRUE)
  names(face_value) <- bonds$bond_id

  day_count <- match.arg(day_count, c("ACT/ACT", "ACT/365.25", "ACT/365"))
  prepared <- prepare_dirty_price_data(
    bonds = bonds,
    dirty_prices = dirty_prices,
    face_value = face_value
  )

  if (nrow(prepared) == 0) {
    prepared$accrued_interest <- numeric()
    prepared$clean_price <- numeric()
    return(prepared)
  }

  prepared$accrued_interest <- vapply(seq_len(nrow(prepared)), function(i) {
    accrued_interest_for_row(
      bond = prepared[i, , drop = FALSE],
      settlement_date = prepared$date[i],
      face_value = prepared$face_value[i],
      day_count = day_count
    )
  }, numeric(1))

  prepared$clean_price <- prepared$dirty_price - prepared$accrued_interest
  if (any(!is.finite(prepared$clean_price)) || any(prepared$clean_price <= 0)) {
    stop("Calculated clean prices must be positive and finite.", call. = FALSE)
  }
  prepared
}

#' Calculate Yield to Maturity from Prices
#'
#' Calculates bond-level yield to maturity from quoted clean prices. The
#' function first computes dirty prices, then solves for the single annualized
#' yield that discounts each bond's remaining cash flows to its dirty price.
#'
#' @param bonds A data frame with columns `bond_id`, `issue_date`, `maturity`,
#'   `coupon_rate`, and `coupon_frequency`.
#' @param quotes A wide clean-price data frame accepted by [validate_quotes()].
#' @param face_value Numeric face value. Use a single value for all bonds or one
#'   value per row of `bonds`. Defaults to `100`.
#' @param accrued_day_count Day-count convention for accrued interest passed to
#'   [calculate_dirty_prices()].
#' @param discount_day_count Day-count convention for discounting remaining cash
#'   flows. Currently `"ACT/365.25"` and `"ACT/365"` are supported.
#' @param compounding Compounding convention. Currently `"annual"` and
#'   `"continuous"` are supported.
#' @param progress Logical. If `TRUE`, show a text progress bar while
#'   yield-to-maturity values are calculated.
#'
#' @return A data frame containing dirty-price data plus `ytm`.
#' @export
#'
#' @examples
#' bonds <- data.frame(
#'   bond_id = "B1",
#'   issue_date = as.Date("2026-01-01"),
#'   maturity = as.Date("2028-01-01"),
#'   coupon_rate = 0.04,
#'   coupon_frequency = 2L
#' )
#' quotes <- data.frame(
#'   date = as.Date("2026-04-01"),
#'   B1 = 99
#' )
#' calculate_ytm(bonds, quotes)
calculate_ytm <- function(bonds,
                          quotes,
                          face_value = 100,
                          accrued_day_count = "ACT/ACT",
                          discount_day_count = "ACT/365.25",
                          compounding = "annual",
                          progress = FALSE) {
  prices <- calculate_dirty_price_data_frame(
    bonds = bonds,
    quotes = quotes,
    face_value = face_value,
    day_count = accrued_day_count
  )

  if (nrow(prices) == 0) {
    prices$ytm <- numeric()
    return(prices)
  }

  compounding <- match.arg(compounding, c("annual", "continuous"))
  progress_bar <- create_progress_bar(progress, nrow(prices), "Calculating yield to maturity")
  on.exit(close_progress_bar(progress_bar), add = TRUE)

  prices$ytm <- vapply(seq_len(nrow(prices)), function(i) {
    out <- solve_ytm_for_row(
      row = prices[i, , drop = FALSE],
      discount_day_count = discount_day_count,
      compounding = compounding
    )
    update_progress_bar(progress_bar, i)
    out
  }, numeric(1))

  prices
}

#' Calculate Bond Yields from Clean Prices
#'
#' Calculates bond-level yield-to-maturity values from quoted clean prices.
#' This is a convenience wrapper around [calculate_ytm()] intended for workflows
#' where the same yields are reused across several estimation functions.
#'
#' @param bonds A data frame with columns `bond_id`, `issue_date`, `maturity`,
#'   `coupon_rate`, and `coupon_frequency`.
#' @param quotes A wide clean-price data frame accepted by [validate_quotes()].
#' @param face_value Numeric face value. Use a single value for all bonds or one
#'   value per row of `bonds`. Defaults to `100`.
#' @param accrued_day_count Day-count convention for accrued interest passed to
#'   [calculate_dirty_prices()].
#' @param discount_day_count Day-count convention for discounting remaining cash
#'   flows. Currently `"ACT/365.25"` and `"ACT/365"` are supported.
#' @param compounding Compounding convention. Currently `"annual"` and
#'   `"continuous"` are supported.
#' @param progress Logical. If `TRUE`, show a text progress bar while yields
#'   are calculated.
#'
#' @return A data frame containing dirty-price data plus `ytm`.
#' @export
#'
#' @examples
#' bonds <- data.frame(
#'   bond_id = "B1",
#'   issue_date = as.Date("2026-01-01"),
#'   maturity = as.Date("2028-01-01"),
#'   coupon_rate = 0.04,
#'   coupon_frequency = 2L
#' )
#' quotes <- data.frame(date = as.Date("2026-04-01"), B1 = 99)
#' yields <- calculate_yields(bonds, quotes)
calculate_yields <- function(bonds,
                             quotes,
                             face_value = 100,
                             accrued_day_count = "ACT/ACT",
                             discount_day_count = "ACT/365.25",
                             compounding = "annual",
                             progress = FALSE) {
  prices <- calculate_dirty_price_data_frame(
    bonds = bonds,
    quotes = quotes,
    face_value = face_value,
    day_count = accrued_day_count
  )

  if (nrow(prices) == 0) {
    prices$ytm <- numeric()
    return(prices)
  }

  compounding <- match.arg(compounding, c("annual", "continuous"))
  progress_bar <- create_progress_bar(progress, nrow(prices), "Calculating yields")
  on.exit(close_progress_bar(progress_bar), add = TRUE)

  prices$ytm <- vapply(seq_len(nrow(prices)), function(i) {
    out <- solve_ytm_for_row(
      row = prices[i, , drop = FALSE],
      discount_day_count = discount_day_count,
      compounding = compounding
    )
    update_progress_bar(progress_bar, i)
    out
  }, numeric(1))

  prices
}

#' Estimate NSS Curve from Clean Price Quotes
#'
#' Runs the standard price-based NSS estimation workflow: validate inputs,
#' convert clean prices to dirty prices, optionally calculate yield-to-maturity
#' diagnostics, and fit NSS parameters directly to dirty prices.
#'
#' @param bonds A data frame with columns `bond_id`, `issue_date`, `maturity`,
#'   `coupon_rate`, and `coupon_frequency`.
#' @param quotes A wide clean-price data frame accepted by [validate_quotes()].
#' @param start Optional named numeric vector with starting values for `beta0`,
#'   `beta1`, `beta2`, `beta3`, `lambda1`, and `lambda2`.
#' @param face_value Numeric face value. Use a single value for all bonds or one
#'   value per row of `bonds`. Defaults to `100`.
#' @param accrued_day_count Day-count convention for accrued interest passed to
#'   [calculate_dirty_prices()].
#' @param discount_day_count Day-count convention for discounting cash flows.
#' @param compounding Compounding convention. Currently `"annual"` and
#'   `"continuous"` are supported.
#' @param include_ytm Logical. If `TRUE`, include yield-to-maturity diagnostics
#'   in the returned object.
#' @param yields Optional data frame previously returned by [calculate_yields()]
#'   or [calculate_ytm()]. If supplied and `include_ytm = TRUE`, these values are
#'   reused instead of being calculated again.
#' @param weighting Objective weighting scheme. Use `"none"` for the ordinary
#'   sum of squared dirty-price errors or `"inverse_duration"` for weights
#'   proportional to the inverse bond duration within each quote date.
#' @param progress Logical. If `TRUE`, show text progress bars while
#'   date-by-date NSS price estimation and optional yield-to-maturity
#'   diagnostics are running.
#'
#' @return An S3 object of class `"yc_nss_estimate"` with elements
#'   `dirty_prices`, `fit`, and optionally `ytm`.
#' @export
estimate_nss_from_quotes <- function(bonds,
                                     quotes,
                                     start = NULL,
                                     face_value = 100,
                                     accrued_day_count = "ACT/ACT",
                                     discount_day_count = "ACT/365.25",
                                     compounding = "annual",
                                     include_ytm = TRUE,
                                     yields = NULL,
                                     weighting = c("none", "inverse_duration"),
                                     progress = FALSE) {
  weighting <- match.arg(weighting)
  dirty_prices <- calculate_dirty_prices(
    bonds = bonds,
    quotes = quotes,
    face_value = face_value,
    day_count = accrued_day_count
  )

  fit <- fit_nss_dirty_prices(
    bonds = bonds,
    dirty_prices = dirty_prices,
    start = start,
    face_value = face_value,
    discount_day_count = discount_day_count,
    compounding = compounding,
    weighting = weighting,
    progress = progress
  )
  fit$settings$accrued_day_count <- accrued_day_count

  out <- list(
    dirty_prices = dirty_prices,
    fit = fit,
    settings = list(
      accrued_day_count = accrued_day_count,
      discount_day_count = discount_day_count,
      compounding = compounding,
      weighting = weighting
    )
  )

  if (isTRUE(include_ytm)) {
    out$ytm <- yields_for_estimation(
      yields = yields,
      bonds = bonds,
      quotes = quotes,
      face_value = face_value,
      accrued_day_count = accrued_day_count,
      discount_day_count = discount_day_count,
      compounding = compounding,
      progress = progress
    )
  }

  structure(out, class = "yc_nss_estimate")
}

#' Estimate NS Curve from Clean Price Quotes
#'
#' Runs the standard price-based Nelson-Siegel estimation workflow: validate
#' inputs, convert clean prices to dirty prices, optionally calculate
#' yield-to-maturity diagnostics, and fit NS parameters directly to dirty
#' prices.
#'
#' @param bonds A data frame with columns `bond_id`, `issue_date`, `maturity`,
#'   `coupon_rate`, and `coupon_frequency`.
#' @param quotes A wide clean-price data frame accepted by [validate_quotes()].
#' @param start Optional named numeric vector with starting values for `beta0`,
#'   `beta1`, `beta2`, and `lambda`.
#' @param face_value Numeric face value. Use a single value for all bonds or one
#'   value per row of `bonds`. Defaults to `100`.
#' @param accrued_day_count Day-count convention for accrued interest passed to
#'   [calculate_dirty_prices()].
#' @param discount_day_count Day-count convention for discounting cash flows.
#' @param compounding Compounding convention. Currently `"annual"` and
#'   `"continuous"` are supported.
#' @param include_ytm Logical. If `TRUE`, include yield-to-maturity diagnostics
#'   in the returned object.
#' @param yields Optional data frame previously returned by [calculate_yields()]
#'   or [calculate_ytm()]. If supplied and `include_ytm = TRUE`, these values are
#'   reused instead of being calculated again.
#' @param weighting Objective weighting scheme. Use `"none"` for the ordinary
#'   sum of squared dirty-price errors or `"inverse_duration"` for weights
#'   proportional to the inverse bond duration within each quote date.
#' @param progress Logical. If `TRUE`, show text progress bars while
#'   date-by-date NS price estimation and optional yield-to-maturity
#'   diagnostics are running.
#'
#' @return An S3 object of class `"yc_ns_estimate"` with elements
#'   `dirty_prices`, `fit`, and optionally `ytm`.
#' @export
estimate_ns_from_quotes <- function(bonds,
                                    quotes,
                                    start = NULL,
                                    face_value = 100,
                                    accrued_day_count = "ACT/ACT",
                                    discount_day_count = "ACT/365.25",
                                    compounding = "annual",
                                    include_ytm = TRUE,
                                    yields = NULL,
                                    weighting = c("none", "inverse_duration"),
                                    progress = FALSE) {
  weighting <- match.arg(weighting)
  dirty_prices <- calculate_dirty_prices(
    bonds = bonds,
    quotes = quotes,
    face_value = face_value,
    day_count = accrued_day_count
  )

  fit <- fit_ns_dirty_prices(
    bonds = bonds,
    dirty_prices = dirty_prices,
    start = start,
    face_value = face_value,
    discount_day_count = discount_day_count,
    compounding = compounding,
    weighting = weighting,
    progress = progress
  )
  fit$settings$accrued_day_count <- accrued_day_count

  out <- list(
    dirty_prices = dirty_prices,
    fit = fit,
    settings = list(
      accrued_day_count = accrued_day_count,
      discount_day_count = discount_day_count,
      compounding = compounding,
      weighting = weighting
    )
  )

  if (isTRUE(include_ytm)) {
    out$ytm <- yields_for_estimation(
      yields = yields,
      bonds = bonds,
      quotes = quotes,
      face_value = face_value,
      accrued_day_count = accrued_day_count,
      discount_day_count = discount_day_count,
      compounding = compounding,
      progress = progress
    )
  }

  structure(out, class = "yc_ns_estimate")
}

#' Fit Nelson-Siegel Model from Clean Prices
#'
#' Estimates Nelson-Siegel parameters directly from quoted clean prices. The
#' function first converts clean prices to dirty prices, then minimizes
#' dirty-price residuals using cash-flow present values under the NS zero curve.
#' For direct fitting to precomputed dirty prices, use
#' [fit_ns_dirty_prices()].
#'
#' @param bonds A data frame with columns `bond_id`, `issue_date`, `maturity`,
#'   `coupon_rate`, and `coupon_frequency`.
#' @param quotes A wide clean-price data frame accepted by [validate_quotes()].
#' @param start Optional named numeric vector with starting values for `beta0`,
#'   `beta1`, `beta2`, and `lambda`.
#' @param face_value Numeric face value. Use a single value for all bonds or one
#'   value per row of `bonds`. Defaults to `100`.
#' @param accrued_day_count Day-count convention for accrued interest passed to
#'   [calculate_dirty_prices()].
#' @param discount_day_count Day-count convention for discounting cash flows.
#' @param compounding Compounding convention. Currently `"annual"` and
#'   `"continuous"` are supported.
#' @param weighting Objective weighting scheme. Use `"none"` for the ordinary
#'   sum of squared dirty-price errors or `"inverse_duration"` for weights
#'   proportional to the inverse bond duration within each quote date.
#' @param progress Logical. If `TRUE`, show a text progress bar while dates are
#'   fitted sequentially.
#'
#' @return An S3 object of class `"yc_ns_price"`.
#' @export
fit_ns_prices <- function(bonds,
                          quotes,
                          start = NULL,
                          face_value = 100,
                          accrued_day_count = "ACT/ACT",
                          discount_day_count = "ACT/365.25",
                          compounding = "annual",
                          weighting = c("none", "inverse_duration"),
                          progress = FALSE) {
  weighting <- match.arg(weighting)
  dirty_prices <- calculate_dirty_prices(
    bonds = bonds,
    quotes = quotes,
    face_value = face_value,
    day_count = accrued_day_count
  )

  fit <- fit_ns_dirty_prices(
    bonds = bonds,
    dirty_prices = dirty_prices,
    start = start,
    face_value = face_value,
    discount_day_count = discount_day_count,
    compounding = compounding,
    weighting = weighting,
    progress = progress
  )
  fit$settings$accrued_day_count <- accrued_day_count
  fit
}

#' Fit Nelson-Siegel Model from Dirty Prices
#'
#' Estimates Nelson-Siegel parameters directly from dirty prices. For each
#' quote date, theoretical prices are computed as discounted contractual cash
#' flows. Discount rates are NS zero rates evaluated at each cash-flow maturity.
#' The objective minimizes the sum of squared dirty-price errors.
#'
#' Dates are fitted sequentially. The first date uses `start` or a default
#' derived from same-day yield-to-maturity values; each next date uses the
#' fitted NS coefficients from the previous date as its starting values. For
#' robustness, the optimizer also checks the same-day yield-based start and
#' keeps the solution with the lower price objective.
#'
#' @param bonds A data frame with columns `bond_id`, `issue_date`, `maturity`,
#'   `coupon_rate`, and `coupon_frequency`.
#' @param dirty_prices A wide dirty-price data frame. The first column must be
#'   `date`; each remaining column is one bond.
#' @param start Optional named numeric vector with starting values for `beta0`,
#'   `beta1`, `beta2`, and `lambda`.
#' @param face_value Numeric face value. Use a single value for all bonds or one
#'   value per row of `bonds`. Defaults to `100`.
#' @param discount_day_count Day-count convention for discounting cash flows.
#' @param compounding Compounding convention. Currently `"annual"` and
#'   `"continuous"` are supported.
#' @param weighting Objective weighting scheme. Use `"none"` for the ordinary
#'   sum of squared dirty-price errors or `"inverse_duration"` for weights
#'   proportional to the inverse bond duration within each quote date.
#'
#' @return An S3 object of class `"yc_ns_price"`.
#' @export
fit_ns_dirty_prices <- function(bonds,
                                dirty_prices,
                                start = NULL,
                                face_value = 100,
                                discount_day_count = "ACT/365.25",
                                compounding = "annual",
                                weighting = c("none", "inverse_duration"),
                                progress = FALSE) {
  weighting <- match.arg(weighting)
  validate_bonds(bonds)
  validate_dirty_prices(dirty_prices)

  face_value <- normalize_bond_vector(face_value, bonds$bond_id, "face_value", positive = TRUE)
  names(face_value) <- bonds$bond_id

  price_data <- prepare_dirty_price_data(
    bonds = bonds,
    dirty_prices = dirty_prices,
    face_value = face_value
  )
  if (nrow(price_data) < 4) {
    stop("At least four dirty-price observations are required to fit NS from prices.", call. = FALSE)
  }

  compounding <- match.arg(compounding, c("annual", "continuous"))
  dates <- sort(unique(price_data$date))

  fits <- vector("list", length(dates))
  start_sources <- character(length(dates))
  current_start <- start
  progress_bar <- create_progress_bar(progress, length(dates), "Estimating NS prices")
  on.exit(close_progress_bar(progress_bar), add = TRUE)

  for (i in seq_along(dates)) {
    d <- dates[i]
    start_sources[i] <- if (i == 1) {
      if (is.null(start)) "default" else "user"
    } else {
      "previous_date"
    }
    fits[[i]] <- fit_ns_prices_one_date(
      price_data = price_data[price_data$date == d, , drop = FALSE],
      start = current_start,
      discount_day_count = discount_day_count,
      compounding = compounding,
      weighting = weighting
    )
    current_start <- unlist(fits[[i]]$coefficients[1, c(
      "beta0", "beta1", "beta2", "lambda"
    )])
    update_progress_bar(progress_bar, i)
  }

  coefficients <- do.call(rbind, lapply(fits, function(x) x$coefficients))
  coefficients <- data.frame(date = dates, coefficients, row.names = NULL)

  fitted_values <- do.call(rbind, lapply(fits, function(x) x$fitted_values))
  rownames(fitted_values) <- NULL

  convergence <- data.frame(
    date = dates,
    code = vapply(fits, function(x) x$convergence$code, integer(1)),
    value = vapply(fits, function(x) x$convergence$value, numeric(1)),
    start_source = start_sources
  )

  structure(
    list(
      coefficients = coefficients,
      fitted_values = fitted_values,
      residuals = fitted_values$dirty_price - fitted_values$fitted_dirty_price,
      observed = price_data,
      model = "Nelson-Siegel price fit",
      convergence = convergence,
      settings = list(
        discount_day_count = discount_day_count,
        compounding = compounding,
        weighting = weighting
      )
    ),
    class = "yc_ns_price"
  )
}

#' Fit Nelson-Siegel-Svensson Model from Clean Prices
#'
#' Estimates Nelson-Siegel-Svensson parameters directly from quoted clean
#' prices. The objective minimizes dirty-price errors after converting clean
#' prices to dirty prices and pricing each bond from NSS zero rates applied to
#' its remaining cash flows. For direct fitting to precomputed dirty prices, use
#' [fit_nss_dirty_prices()].
#'
#' @param bonds A data frame with columns `bond_id`, `issue_date`, `maturity`,
#'   `coupon_rate`, and `coupon_frequency`.
#' @param quotes A wide clean-price data frame accepted by [validate_quotes()].
#' @param start Optional named numeric vector with starting values for `beta0`,
#'   `beta1`, `beta2`, `beta3`, `lambda1`, and `lambda2`.
#' @param face_value Numeric face value. Use a single value for all bonds or one
#'   value per row of `bonds`. Defaults to `100`.
#' @param accrued_day_count Day-count convention for accrued interest passed to
#'   [calculate_dirty_prices()].
#' @param discount_day_count Day-count convention for discounting cash flows.
#' @param compounding Compounding convention. Currently `"annual"` and
#'   `"continuous"` are supported.
#' @param weighting Objective weighting scheme. Use `"none"` for the ordinary
#'   sum of squared dirty-price errors or `"inverse_duration"` for weights
#'   proportional to the inverse bond duration within each quote date.
#' @param progress Logical. If `TRUE`, show a text progress bar while dates are
#'   fitted sequentially.
#'
#' @return An S3 object of class `"yc_nss_price"`.
#' @export
fit_nss_prices <- function(bonds,
                           quotes,
                           start = NULL,
                           face_value = 100,
                           accrued_day_count = "ACT/ACT",
                           discount_day_count = "ACT/365.25",
                           compounding = "annual",
                           weighting = c("none", "inverse_duration"),
                           progress = FALSE) {
  weighting <- match.arg(weighting)
  dirty_prices <- calculate_dirty_prices(
    bonds = bonds,
    quotes = quotes,
    face_value = face_value,
    day_count = accrued_day_count
  )

  fit <- fit_nss_dirty_prices(
    bonds = bonds,
    dirty_prices = dirty_prices,
    start = start,
    face_value = face_value,
    discount_day_count = discount_day_count,
    compounding = compounding,
    weighting = weighting,
    progress = progress
  )
  fit$settings$accrued_day_count <- accrued_day_count
  fit
}

#' Fit Nelson-Siegel-Svensson Model from Dirty Prices
#'
#' Estimates Nelson-Siegel-Svensson parameters directly from dirty prices. For
#' each quote date, theoretical prices are computed as discounted contractual
#' cash flows. Discount rates are NSS zero rates evaluated at each cash-flow
#' maturity. The objective minimizes the sum of squared dirty-price errors.
#'
#' Dates are fitted sequentially. The first date uses `start` or a default
#' derived from same-day yield-to-maturity values; each next date uses the
#' fitted NSS coefficients from the previous date as its starting values. For
#' robustness, the optimizer also checks the same-day yield-based start and
#' keeps the solution with the lower price objective.
#'
#' @param bonds A data frame with columns `bond_id`, `issue_date`, `maturity`,
#'   `coupon_rate`, and `coupon_frequency`.
#' @param dirty_prices A wide dirty-price data frame. The first column must be
#'   `date`; each remaining column is one bond.
#' @param start Optional named numeric vector with starting values for `beta0`,
#'   `beta1`, `beta2`, `beta3`, `lambda1`, and `lambda2`.
#' @param face_value Numeric face value. Use a single value for all bonds or one
#'   value per row of `bonds`. Defaults to `100`.
#' @param discount_day_count Day-count convention for discounting cash flows.
#' @param compounding Compounding convention. Currently `"annual"` and
#'   `"continuous"` are supported.
#' @param weighting Objective weighting scheme. Use `"none"` for the ordinary
#'   sum of squared dirty-price errors or `"inverse_duration"` for weights
#'   proportional to the inverse bond duration within each quote date.
#'
#' @return An S3 object of class `"yc_nss_price"`.
#' @export
fit_nss_dirty_prices <- function(bonds,
                                 dirty_prices,
                                 start = NULL,
                                 face_value = 100,
                                 discount_day_count = "ACT/365.25",
                                 compounding = "annual",
                                 weighting = c("none", "inverse_duration"),
                                 progress = FALSE) {
  weighting <- match.arg(weighting)
  validate_bonds(bonds)
  validate_dirty_prices(dirty_prices)

  face_value <- normalize_bond_vector(face_value, bonds$bond_id, "face_value", positive = TRUE)
  names(face_value) <- bonds$bond_id

  price_data <- prepare_dirty_price_data(
    bonds = bonds,
    dirty_prices = dirty_prices,
    face_value = face_value
  )
  if (nrow(price_data) < 6) {
    stop("At least six dirty-price observations are required to fit NSS from prices.", call. = FALSE)
  }

  compounding <- match.arg(compounding, c("annual", "continuous"))
  dates <- sort(unique(price_data$date))

  fits <- vector("list", length(dates))
  start_sources <- character(length(dates))
  current_start <- start
  progress_bar <- create_progress_bar(progress, length(dates), "Estimating NSS prices")
  on.exit(close_progress_bar(progress_bar), add = TRUE)

  for (i in seq_along(dates)) {
    d <- dates[i]
    start_sources[i] <- if (i == 1) {
      if (is.null(start)) "default" else "user"
    } else {
      "previous_date"
    }
    fits[[i]] <- fit_nss_prices_one_date(
      price_data = price_data[price_data$date == d, , drop = FALSE],
      start = current_start,
      discount_day_count = discount_day_count,
      compounding = compounding,
      weighting = weighting
    )
    current_start <- unlist(fits[[i]]$coefficients[1, c(
      "beta0", "beta1", "beta2", "beta3", "lambda1", "lambda2"
    )])
    update_progress_bar(progress_bar, i)
  }

  coefficients <- do.call(rbind, lapply(fits, function(x) x$coefficients))
  coefficients <- data.frame(date = dates, coefficients, row.names = NULL)

  fitted_values <- do.call(rbind, lapply(fits, function(x) x$fitted_values))
  rownames(fitted_values) <- NULL

  convergence <- data.frame(
    date = dates,
    code = vapply(fits, function(x) x$convergence$code, integer(1)),
    value = vapply(fits, function(x) x$convergence$value, numeric(1)),
    start_source = start_sources
  )

  structure(
    list(
      coefficients = coefficients,
      fitted_values = fitted_values,
      residuals = fitted_values$dirty_price - fitted_values$fitted_dirty_price,
      observed = price_data,
      model = "Nelson-Siegel-Svensson price fit",
      convergence = convergence,
      settings = list(
        discount_day_count = discount_day_count,
        compounding = compounding,
        weighting = weighting
      )
    ),
    class = "yc_nss_price"
  )
}

validate_dirty_prices <- function(dirty_prices) {
  validate_price_panel(dirty_prices, price_type = "dirty")
  invisible(TRUE)
}

dirty_prices_to_data_frame <- function(dirty_prices) {
  validate_dirty_prices(dirty_prices)
  price_panel_to_long(dirty_prices, "dirty_price")
}

prepare_dirty_price_data <- function(bonds, dirty_prices, face_value) {
  dirty_data <- dirty_prices_to_data_frame(dirty_prices)
  unknown_ids <- setdiff(unique(dirty_data$bond_id), bonds$bond_id)
  if (length(unknown_ids) > 0) {
    stop(
      "dirty_prices contains bond_id value(s) not present in bonds: ",
      paste(unknown_ids, collapse = ", "),
      call. = FALSE
    )
  }

  joined <- merge(
    dirty_data,
    bonds,
    by = "bond_id",
    all.x = TRUE,
    all.y = FALSE,
    sort = FALSE
  )
  joined$time_to_maturity <- as.numeric(joined$maturity - joined$date) / 365.25
  joined <- joined[joined$time_to_maturity > 0, , drop = FALSE]
  joined$face_value <- unname(face_value[joined$bond_id])
  rownames(joined) <- NULL
  joined
}

accrued_interest_for_row <- function(bond, settlement_date, face_value, day_count) {
  if (bond$coupon_rate == 0) {
    return(0)
  }

  frequency <- as.integer(bond$coupon_frequency)
  coupon_amount <- face_value * bond$coupon_rate / frequency
  coupon_dates <- coupon_schedule_dates(
    issue_date = bond$issue_date,
    maturity = bond$maturity,
    coupon_frequency = frequency
  )

  if (settlement_date %in% coupon_dates || settlement_date <= bond$issue_date) {
    return(0)
  }

  previous_candidates <- coupon_dates[coupon_dates < settlement_date]
  previous_coupon_date <- if (length(previous_candidates) == 0) {
    bond$issue_date
  } else {
    max(previous_candidates)
  }

  next_candidates <- coupon_dates[coupon_dates > settlement_date]
  if (length(next_candidates) == 0) {
    return(0)
  }
  next_coupon_date <- min(next_candidates)

  elapsed_days <- as.numeric(settlement_date - previous_coupon_date)
  period_days <- as.numeric(next_coupon_date - previous_coupon_date)
  accrual_fraction <- switch(
    day_count,
    "ACT/ACT" = elapsed_days / period_days,
    "ACT/365.25" = year_fraction(previous_coupon_date, settlement_date, "ACT/365.25") * frequency,
    "ACT/365" = year_fraction(previous_coupon_date, settlement_date, "ACT/365") * frequency
  )

  coupon_amount * accrual_fraction
}

accrued_interest_for_dates <- function(bond, settlement_date, face_value, day_count) {
  settlement_date <- as.Date(settlement_date)
  out <- rep(NA_real_, length(settlement_date))
  in_life <- settlement_date >= bond$issue_date & settlement_date < bond$maturity

  if (!any(in_life)) {
    return(out)
  }
  if (bond$coupon_rate == 0) {
    out[in_life] <- 0
    return(out)
  }

  frequency <- as.integer(bond$coupon_frequency)
  coupon_amount <- face_value * bond$coupon_rate / frequency
  coupon_dates <- coupon_schedule_dates(
    issue_date = bond$issue_date,
    maturity = bond$maturity,
    coupon_frequency = frequency
  )

  active_dates <- settlement_date[in_life]
  previous_index <- findInterval(active_dates, coupon_dates)
  previous_coupon_date <- coupon_dates[pmax(previous_index, 1)]
  previous_coupon_date[previous_index == 0] <- bond$issue_date

  next_index <- previous_index + 1
  has_next <- next_index <= length(coupon_dates)
  next_coupon_date <- as.Date(rep(NA, length(active_dates)))
  next_coupon_date[has_next] <- coupon_dates[next_index[has_next]]

  values <- rep(0, length(active_dates))
  accrue <- has_next & !(active_dates %in% coupon_dates) & active_dates > bond$issue_date
  if (any(accrue)) {
    elapsed_days <- as.numeric(active_dates[accrue] - previous_coupon_date[accrue])
    period_days <- as.numeric(next_coupon_date[accrue] - previous_coupon_date[accrue])
    accrual_fraction <- switch(
      day_count,
      "ACT/ACT" = elapsed_days / period_days,
      "ACT/365.25" = year_fraction(previous_coupon_date[accrue], active_dates[accrue], "ACT/365.25") * frequency,
      "ACT/365" = year_fraction(previous_coupon_date[accrue], active_dates[accrue], "ACT/365") * frequency
    )
    values[accrue] <- coupon_amount * accrual_fraction
  }

  out[in_life] <- values
  out
}

solve_ytm_for_row <- function(row, discount_day_count, compounding) {
  cash_flows <- generate_cash_flows(
    bonds = row[, c("bond_id", "issue_date", "maturity", "coupon_rate", "coupon_frequency"), drop = FALSE],
    face_value = row$face_value,
    settlement_date = row$date
  )
  if (nrow(cash_flows) == 0) {
    return(NA_real_)
  }

  t <- year_fraction(row$date, cash_flows$cash_flow_date, discount_day_count)
  target <- row$dirty_price
  objective <- function(yield) {
    sum(cash_flows$cash_flow * discount_factor(yield, t, compounding)) - target
  }

  lower <- -0.99
  upper <- 1
  f_lower <- objective(lower)
  f_upper <- objective(upper)
  if (!is.finite(f_lower) || !is.finite(f_upper)) {
    return(NA_real_)
  }
  while (f_lower * f_upper > 0 && upper < 100) {
    upper <- upper * 2
    f_upper <- objective(upper)
    if (!is.finite(f_upper)) {
      return(NA_real_)
    }
  }
  if (f_lower * f_upper > 0) {
    return(NA_real_)
  }

  stats::uniroot(objective, lower = lower, upper = upper, tol = 1e-12)$root
}

fit_ns_prices_one_date <- function(price_data, start, discount_day_count, compounding, weighting) {
  if (nrow(price_data) < 4) {
    stop(
      "At least four bond price observations are required for date ",
      price_data$date[1],
      ".",
      call. = FALSE
    )
  }

  cash_flow_data <- build_price_cash_flow_data(price_data, discount_day_count)

  default_start <- default_ns_price_start(price_data, cash_flow_data, compounding)
  if (is.null(start)) {
    starts <- list(default_start)
  } else {
    start <- validate_start(start, c("beta0", "beta1", "beta2", "lambda"))
    starts <- list(start, default_start)
  }

  objective_weights <- price_objective_weights(
    price_data = price_data,
    cash_flow_data = cash_flow_data,
    weighting = weighting,
    compounding = compounding
  )
  objective <- function(par) {
    model_prices <- ns_model_prices_from_cash_flows(cash_flow_data, par, compounding)
    if (any(!is.finite(model_prices))) {
      return(.Machine$double.xmax / 1e100)
    }
    observed_prices <- price_data$dirty_price[match(names(model_prices), price_data$bond_id)]
    weights <- objective_weights$weight[match(names(model_prices), objective_weights$bond_id)]
    value <- sum(weights * (model_prices - observed_prices)^2)
    if (!is.finite(value)) {
      return(.Machine$double.xmax / 1e100)
    }
    value
  }

  fit <- choose_best_price_fit(
    starts = starts,
    objective = objective,
    lower = c(beta0 = -0.5, beta1 = -0.5, beta2 = -0.5, lambda = 1e-04),
    upper = c(beta0 = 0.5, beta1 = 0.5, beta2 = 0.5, lambda = 10),
    parscale = c(beta0 = 0.01, beta1 = 0.01, beta2 = 0.01, lambda = 1)
  )

  coefficients <- stats::setNames(fit$par, c("beta0", "beta1", "beta2", "lambda"))
  model_prices <- ns_model_prices_from_cash_flows(cash_flow_data, coefficients, compounding)
  fitted_values <- price_data
  fitted_values$fitted_dirty_price <- unname(model_prices[fitted_values$bond_id])
  fitted_values$price_residual <- fitted_values$dirty_price - fitted_values$fitted_dirty_price
  fitted_values$duration <- objective_weights$duration[match(fitted_values$bond_id, objective_weights$bond_id)]
  fitted_values$objective_weight <- objective_weights$weight[match(fitted_values$bond_id, objective_weights$bond_id)]

  list(
    coefficients = as.data.frame(as.list(coefficients)),
    fitted_values = fitted_values,
    convergence = list(
      code = fit$convergence,
      message = fit$message,
      value = fit$value,
      counts = fit$counts
    )
  )
}

fit_nss_prices_one_date <- function(price_data, start, discount_day_count, compounding, weighting) {
  if (nrow(price_data) < 6) {
    stop(
      "At least six bond price observations are required for date ",
      price_data$date[1],
      ".",
      call. = FALSE
    )
  }

  cash_flow_data <- build_price_cash_flow_data(price_data, discount_day_count)

  default_start <- default_nss_price_start(price_data, cash_flow_data, compounding)
  if (is.null(start)) {
    starts <- list(default_start)
  } else {
    start <- validate_start(
      start,
      c("beta0", "beta1", "beta2", "beta3", "lambda1", "lambda2")
    )
    starts <- list(start, default_start)
  }

  objective_weights <- price_objective_weights(
    price_data = price_data,
    cash_flow_data = cash_flow_data,
    weighting = weighting,
    compounding = compounding
  )
  objective <- function(par) {
    model_prices <- nss_model_prices_from_cash_flows(cash_flow_data, par, compounding)
    if (any(!is.finite(model_prices))) {
      return(.Machine$double.xmax / 1e100)
    }
    observed_prices <- price_data$dirty_price[match(names(model_prices), price_data$bond_id)]
    weights <- objective_weights$weight[match(names(model_prices), objective_weights$bond_id)]
    value <- sum(weights * (model_prices - observed_prices)^2)
    if (!is.finite(value)) {
      return(.Machine$double.xmax / 1e100)
    }
    value
  }

  fit <- choose_best_price_fit(
    starts = starts,
    objective = objective,
    lower = c(
      beta0 = -0.5, beta1 = -0.5, beta2 = -0.5, beta3 = -0.5,
      lambda1 = 1e-04, lambda2 = 1e-04
    ),
    upper = c(
      beta0 = 0.5, beta1 = 0.5, beta2 = 0.5, beta3 = 0.5,
      lambda1 = 10, lambda2 = 10
    ),
    parscale = c(
      beta0 = 0.01, beta1 = 0.01, beta2 = 0.01, beta3 = 0.01,
      lambda1 = 1, lambda2 = 1
    )
  )

  coefficients <- stats::setNames(
    fit$par,
    c("beta0", "beta1", "beta2", "beta3", "lambda1", "lambda2")
  )
  model_prices <- nss_model_prices_from_cash_flows(cash_flow_data, coefficients, compounding)
  fitted_values <- price_data
  fitted_values$fitted_dirty_price <- unname(model_prices[fitted_values$bond_id])
  fitted_values$price_residual <- fitted_values$dirty_price - fitted_values$fitted_dirty_price
  fitted_values$duration <- objective_weights$duration[match(fitted_values$bond_id, objective_weights$bond_id)]
  fitted_values$objective_weight <- objective_weights$weight[match(fitted_values$bond_id, objective_weights$bond_id)]

  list(
    coefficients = as.data.frame(as.list(coefficients)),
    fitted_values = fitted_values,
    convergence = list(
      code = fit$convergence,
      message = fit$message,
      value = fit$value,
      counts = fit$counts
    )
  )
}

default_ns_price_start <- function(price_data, cash_flow_data, compounding) {
  yields <- price_data_implied_yields(price_data, cash_flow_data, compounding)
  start_from_yields(
    maturity = price_data$time_to_maturity,
    yield = yields,
    extra_curvature = FALSE
  )
}

default_nss_price_start <- function(price_data, cash_flow_data, compounding) {
  yields <- price_data_implied_yields(price_data, cash_flow_data, compounding)
  start_from_yields(
    maturity = price_data$time_to_maturity,
    yield = yields,
    extra_curvature = TRUE
  )
}

price_data_implied_yields <- function(price_data, cash_flow_data, compounding) {
  vapply(price_data$bond_id, function(bond_id) {
    cash_flows <- cash_flow_data[cash_flow_data$bond_id == bond_id, , drop = FALSE]
    dirty_price <- price_data$dirty_price[match(bond_id, price_data$bond_id)]
    solve_yield_for_cash_flows(
      cash_flow = cash_flows$cash_flow,
      time = cash_flows$discount_time,
      dirty_price = dirty_price,
      compounding = compounding
    )
  }, numeric(1))
}

start_from_yields <- function(maturity, yield, extra_curvature) {
  ok <- is.finite(maturity) & is.finite(yield) & maturity > 0
  maturity <- maturity[ok]
  yield <- yield[ok]

  if (length(yield) == 0) {
    beta0 <- 0.03
    beta1 <- -0.01
  } else {
    ord <- order(maturity)
    maturity <- maturity[ord]
    yield <- yield[ord]
    long_n <- max(1, ceiling(length(yield) / 3))
    beta0 <- stats::median(utils::tail(yield, long_n))
    beta1 <- yield[1] - beta0
  }

  beta0 <- clamp_numeric(beta0, -0.10, 0.20)
  beta1 <- clamp_numeric(beta1, -0.30, 0.30)

  if (isTRUE(extra_curvature)) {
    return(c(
      beta0 = beta0,
      beta1 = beta1,
      beta2 = 0,
      beta3 = 0,
      lambda1 = 0.5,
      lambda2 = 1.5
    ))
  }

  c(
    beta0 = beta0,
    beta1 = beta1,
    beta2 = 0,
    lambda = 0.5
  )
}

clamp_numeric <- function(x, lower, upper) {
  min(max(x, lower), upper)
}

yields_for_estimation <- function(yields,
                                  bonds,
                                  quotes,
                                  face_value,
                                  accrued_day_count,
                                  discount_day_count,
                                  compounding,
                                  progress) {
  if (is.null(yields)) {
    return(calculate_yields(
      bonds = bonds,
      quotes = quotes,
      face_value = face_value,
      accrued_day_count = accrued_day_count,
      discount_day_count = discount_day_count,
      compounding = compounding,
      progress = progress
    ))
  }

  validate_estimation_yields(yields, bonds = bonds, quotes = quotes)
}

validate_estimation_yields <- function(yields, bonds, quotes = NULL) {
  if (!is.data.frame(yields)) {
    stop("yields must be a data frame returned by calculate_yields() or calculate_ytm().", call. = FALSE)
  }
  required <- c("date", "bond_id", "time_to_maturity", "ytm")
  check_required_columns(yields, required, "yields")

  out <- yields
  out$date <- as.Date(out$date)
  if (any(is.na(out$date))) {
    stop("yields$date must contain valid dates.", call. = FALSE)
  }
  out$bond_id <- as.character(out$bond_id)
  check_numeric_vector(out$time_to_maturity, "yields$time_to_maturity", positive = TRUE)
  if (!is.numeric(out$ytm)) {
    stop("yields$ytm must be numeric.", call. = FALSE)
  }
  if (any(!is.na(out$ytm) & !is.finite(out$ytm))) {
    stop("yields$ytm must contain finite values or NA.", call. = FALSE)
  }

  unknown_ids <- setdiff(unique(out$bond_id), bonds$bond_id)
  if (length(unknown_ids) > 0) {
    stop(
      "yields contains bond_id value(s) not present in bonds: ",
      paste(unknown_ids, collapse = ", "),
      call. = FALSE
    )
  }

  if (!is.null(quotes)) {
    quote_dates <- as.Date(quotes$date)
    unexpected_dates <- setdiff(unique(out$date), unique(quote_dates))
    if (length(unexpected_dates) > 0) {
      stop(
        "yields contains date value(s) not present in quotes: ",
        paste(format(unexpected_dates), collapse = ", "),
        call. = FALSE
      )
    }

    quote_ids <- setdiff(names(quotes), "date")
    unexpected_ids <- setdiff(unique(out$bond_id), quote_ids)
    if (length(unexpected_ids) > 0) {
      stop(
        "yields contains bond_id value(s) not present as quote columns: ",
        paste(unexpected_ids, collapse = ", "),
        call. = FALSE
      )
    }
  }

  out[order(out$date, out$bond_id), , drop = FALSE]
}

choose_best_price_fit <- function(starts, objective, lower, upper, parscale) {
  fits <- lapply(starts, function(start) {
    optimize_price_objective(
      start = start,
      objective = objective,
      lower = lower,
      upper = upper,
      parscale = parscale
    )
  })

  values <- vapply(fits, function(x) x$value, numeric(1))
  fits[[which.min(values)]]
}

optimize_price_objective <- function(start, objective, lower, upper, parscale) {
  start_value <- objective(start)
  if (start_value < 1e-16) {
    return(list(
      par = start,
      convergence = 0L,
      message = "Starting values already minimize the price objective.",
      value = start_value,
      counts = c("function" = 1L, gradient = 0L)
    ))
  }

  stats::optim(
    par = start,
    fn = objective,
    method = "L-BFGS-B",
    lower = lower,
    upper = upper,
    control = list(parscale = parscale)
  )
}

build_price_cash_flow_data <- function(price_data, discount_day_count) {
  rows <- lapply(seq_len(nrow(price_data)), function(i) {
    row <- price_data[i, , drop = FALSE]
    cash_flows <- generate_cash_flows(
      bonds = row[, c("bond_id", "issue_date", "maturity", "coupon_rate", "coupon_frequency"), drop = FALSE],
      face_value = row$face_value,
      settlement_date = row$date
    )
    if (nrow(cash_flows) == 0) {
      stop("No future cash flows for bond ", row$bond_id, " on ", row$date, ".", call. = FALSE)
    }
    cash_flows$discount_time <- year_fraction(row$date, cash_flows$cash_flow_date, discount_day_count)
    cash_flows
  })

  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

price_objective_weights <- function(price_data, cash_flow_data, weighting, compounding) {
  weighting <- match.arg(weighting, c("none", "inverse_duration"))
  bond_ids <- price_data$bond_id

  durations <- vapply(bond_ids, function(bond_id) {
    cash_flows <- cash_flow_data[cash_flow_data$bond_id == bond_id, , drop = FALSE]
    dirty_price <- price_data$dirty_price[match(bond_id, price_data$bond_id)]
    bond_duration(cash_flows, dirty_price, compounding)
  }, numeric(1))

  if (weighting == "none") {
    weights <- rep(1, length(bond_ids))
  } else {
    inverse_duration <- 1 / durations
    weights <- inverse_duration / sum(inverse_duration)
  }

  data.frame(
    bond_id = bond_ids,
    duration = durations,
    weight = weights
  )
}

bond_duration <- function(cash_flows, dirty_price, compounding) {
  ytm <- solve_yield_for_cash_flows(
    cash_flow = cash_flows$cash_flow,
    time = cash_flows$discount_time,
    dirty_price = dirty_price,
    compounding = compounding
  )

  if (is.finite(ytm)) {
    pv <- cash_flows$cash_flow * discount_factor(ytm, cash_flows$discount_time, compounding)
    duration <- sum(cash_flows$discount_time * pv) / sum(pv)
  } else {
    duration <- sum(cash_flows$discount_time * cash_flows$cash_flow) / sum(cash_flows$cash_flow)
  }

  if (!is.finite(duration) || duration <= 0) {
    stop("Could not calculate a positive duration for bond ", cash_flows$bond_id[1], ".", call. = FALSE)
  }
  duration
}

solve_yield_for_cash_flows <- function(cash_flow, time, dirty_price, compounding) {
  objective <- function(yield) {
    sum(cash_flow * discount_factor(yield, time, compounding)) - dirty_price
  }

  lower <- -0.99
  upper <- 1
  f_lower <- objective(lower)
  f_upper <- objective(upper)
  if (!is.finite(f_lower) || !is.finite(f_upper)) {
    return(NA_real_)
  }
  while (f_lower * f_upper > 0 && upper < 100) {
    upper <- upper * 2
    f_upper <- objective(upper)
    if (!is.finite(f_upper)) {
      return(NA_real_)
    }
  }
  if (f_lower * f_upper > 0) {
    return(NA_real_)
  }

  stats::uniroot(objective, lower = lower, upper = upper, tol = 1e-12)$root
}

ns_model_prices_from_cash_flows <- function(cash_flow_data, coefficients, compounding) {
  zero_rates <- ns_yield(
    cash_flow_data$discount_time,
    coefficients["beta0"],
    coefficients["beta1"],
    coefficients["beta2"],
    coefficients["lambda"]
  )
  if (any(!is.finite(zero_rates))) {
    return(stats::setNames(
      rep(Inf, length(unique(cash_flow_data$bond_id))),
      unique(cash_flow_data$bond_id)
    ))
  }

  pv <- cash_flow_data$cash_flow *
    discount_factor(zero_rates, cash_flow_data$discount_time, compounding)
  if (any(!is.finite(pv))) {
    return(stats::setNames(
      rep(Inf, length(unique(cash_flow_data$bond_id))),
      unique(cash_flow_data$bond_id)
    ))
  }

  stats::setNames(
    as.numeric(tapply(pv, cash_flow_data$bond_id, sum)),
    names(tapply(pv, cash_flow_data$bond_id, sum))
  )
}

nss_model_prices_from_cash_flows <- function(cash_flow_data, coefficients, compounding) {
  zero_rates <- nss_yield(
    cash_flow_data$discount_time,
    coefficients["beta0"],
    coefficients["beta1"],
    coefficients["beta2"],
    coefficients["beta3"],
    coefficients["lambda1"],
    coefficients["lambda2"]
  )
  if (any(!is.finite(zero_rates))) {
    return(stats::setNames(
      rep(Inf, length(unique(cash_flow_data$bond_id))),
      unique(cash_flow_data$bond_id)
    ))
  }

  pv <- cash_flow_data$cash_flow *
    discount_factor(zero_rates, cash_flow_data$discount_time, compounding)
  if (any(!is.finite(pv))) {
    return(stats::setNames(
      rep(Inf, length(unique(cash_flow_data$bond_id))),
      unique(cash_flow_data$bond_id)
    ))
  }

  stats::setNames(
    as.numeric(tapply(pv, cash_flow_data$bond_id, sum)),
    names(tapply(pv, cash_flow_data$bond_id, sum))
  )
}

discount_factor <- function(yield, time, compounding) {
  n <- max(length(yield), length(time))
  yield <- rep(yield, length.out = n)
  time <- rep(time, length.out = n)

  if (compounding == "continuous") {
    return(exp(-yield * time))
  }

  out <- rep(Inf, n)
  valid <- is.finite(yield) & is.finite(time) & yield > -1
  out[valid] <- (1 + yield[valid])^(-time[valid])
  out
}

normalize_bond_vector <- function(x, bond_id, name, positive) {
  if (!is.numeric(x) || any(!is.finite(x))) {
    stop(name, " must contain finite numeric values.", call. = FALSE)
  }
  if (positive && any(x <= 0)) {
    stop(name, " must contain positive values.", call. = FALSE)
  }

  if (length(x) == 1) {
    return(rep(x, length(bond_id)))
  }

  if (!is.null(names(x)) && all(bond_id %in% names(x))) {
    return(unname(x[bond_id]))
  }

  if (length(x) == length(bond_id)) {
    return(unname(x))
  }

  stop(
    name,
    " must have length 1, one value per row of bonds, or names matching bonds$bond_id.",
    call. = FALSE
  )
}
