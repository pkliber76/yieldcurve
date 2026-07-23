#' Diebold-Li Factor Loadings
#'
#' Computes the level, slope, and curvature loadings used by the dynamic
#' Nelson-Siegel model.
#'
#' @param maturity Numeric vector of maturities in years.
#' @param lambda Positive fixed decay parameter.
#'
#' @return A numeric matrix with columns `level`, `slope`, and `curvature`.
#' @export
dl_loadings <- function(maturity, lambda) {
  maturity <- as_maturity(maturity)
  validate_lambda(lambda)

  cbind(
    level = rep(1, length(maturity)),
    slope = ns_l1(maturity, lambda),
    curvature = ns_l2(maturity, lambda)
  )
}

#' Fit Diebold-Li Factors by Date
#'
#' For each quotation date, estimates level, slope, and curvature by ordinary
#' least squares using a fixed Nelson-Siegel decay parameter.
#'
#' @param data A data frame with columns `date`, `maturity`, and `yield`.
#' @param lambda Positive fixed decay parameter. Defaults to `0.0609`.
#' @param progress Logical. If `TRUE`, show a text progress bar while factors
#'   are estimated by date.
#'
#' @return A data frame with columns `date`, `level`, `slope`, and `curvature`.
#' @export
fit_dl_factors <- function(data, lambda = 0.0609, progress = FALSE) {
  if (!is.data.frame(data)) {
    stop("data must be a data.frame.", call. = FALSE)
  }
  check_required_columns(data, c("date", "maturity", "yield"), "data")
  if (!inherits(data$date, "Date")) {
    stop("data$date must be a Date column.", call. = FALSE)
  }
  if (any(is.na(data$date))) {
    stop("data$date must not contain missing values.", call. = FALSE)
  }
  check_numeric_vector(as.numeric(data$maturity), "data$maturity", positive = TRUE)
  check_numeric_vector(as.numeric(data$yield), "data$yield")
  validate_lambda(lambda)

  dates <- sort(unique(data$date))
  progress_bar <- create_progress_bar(progress, length(dates), "Estimating Diebold-Li factors")
  on.exit(close_progress_bar(progress_bar), add = TRUE)

  rows <- lapply(seq_along(dates), function(i) {
    d <- dates[i]
    subset <- data[data$date == d, , drop = FALSE]
    if (nrow(subset) < 3) {
      stop("At least three observations are required for date ", d, ".", call. = FALSE)
    }

    x <- dl_loadings(subset$maturity, lambda)
    fit <- lm.fit(x, subset$yield)
    if (fit$rank < 3) {
      stop("Diebold-Li loading matrix is rank deficient for date ", d, ".", call. = FALSE)
    }

    row <- data.frame(
      date = d,
      level = unname(fit$coefficients[1]),
      slope = unname(fit$coefficients[2]),
      curvature = unname(fit$coefficients[3])
    )
    update_progress_bar(progress_bar, i)
    row
  })

  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

#' Plot Diebold-Li Factors
#'
#' Plots level, slope, and curvature factor time series.
#'
#' @param factors A data frame returned by [fit_dl_factors()].
#' @param ... Additional graphical arguments passed to [graphics::matplot()],
#'   such as `col`, `main`, `xlab`, `ylab`, `lwd`, or `lty`.
#'
#' @return Invisibly returns `factors`.
#' @export
plot_dl_factors <- function(factors, ...) {
  if (!is.data.frame(factors)) {
    stop("factors must be a data.frame.", call. = FALSE)
  }
  check_required_columns(factors, c("date", "level", "slope", "curvature"), "factors")

  y <- as.matrix(factors[, c("level", "slope", "curvature")])
  old_par <- setup_yc_plot_par()
  on.exit(graphics::par(old_par), add = TRUE)
  graphics::par(mar = c(4.6, 4.8, 3.4, 12), xpd = NA)
  cols <- yc_plot_palette()
  line_cols <- c(cols$curve, cols$curve_2, cols$curve_3)
  plot_args <- yc_plot_args(list(
    type = "l",
    lty = 1,
    lwd = 2.2,
    col = line_cols,
    xlab = "Date",
    ylab = "Factor",
    main = "Diebold-Li Factors",
    bty = "n"
  ), ...)
  do.call(
    graphics::matplot,
    c(list(x = factors$date, y = y), plot_args)
  )
  add_yc_grid()
  graphics::matlines(factors$date, y, lty = plot_args$lty, lwd = plot_args$lwd, col = plot_args$col)
  usr <- graphics::par("usr")
  graphics::legend(
    x = usr[2] + 0.08 * diff(usr[1:2]),
    y = usr[4],
    xjust = 0,
    yjust = 1,
    legend = colnames(y),
    col = plot_args$col,
    lty = rep(plot_args$lty, length.out = ncol(y)),
    lwd = rep(plot_args$lwd, length.out = ncol(y)),
    bty = "n",
    cex = 0.9,
    y.intersp = 1.15
  )
  invisible(factors)
}

#' Estimate Diebold-Li Factors from Clean Price Quotes
#'
#' Runs a Diebold-Li estimation workflow from the package's bond and quote structures:
#' validate inputs, convert clean prices to dirty prices, and estimate daily
#' Diebold-Li level, slope, and curvature factors by minimizing dirty-price
#' errors using discounted bond cash flows. Yield-to-maturity observations are
#' still calculated as diagnostics and for plotting observed bond points.
#'
#' @param bonds A data frame with columns `bond_id`, `issue_date`, `maturity`,
#'   `coupon_rate`, and `coupon_frequency`.
#' @param quotes A wide clean-price data frame accepted by [validate_quotes()].
#' @param lambda Positive fixed Diebold-Li decay parameter. Defaults to
#'   `0.0609`.
#' @param face_value Numeric face value. Use a single value for all bonds or one
#'   value per row of `bonds`. Defaults to `100`.
#' @param accrued_day_count Day-count convention for accrued interest passed to
#'   [calculate_dirty_prices()].
#' @param discount_day_count Day-count convention for discounting remaining cash
#'   flows.
#' @param compounding Compounding convention. Currently `"annual"` and
#'   `"continuous"` are supported.
#' @param yields Optional data frame previously returned by [calculate_yields()]
#'   or [calculate_ytm()]. If supplied, these values are reused instead of being
#'   calculated again.
#' @param weighting Objective weighting scheme. Use `"none"` for the ordinary
#'   sum of squared dirty-price errors or `"inverse_duration"` for weights
#'   proportional to the inverse bond duration within each quote date.
#' @param progress Logical. If `TRUE`, show text progress bars while
#'   date-by-date Diebold-Li price estimation and yield-to-maturity diagnostics
#'   are running.
#'
#' @return An S3 object of class `"yc_dl_estimate"` with elements
#'   `dirty_prices`, `ytm`, `factor_input`, `factors`, `fitted_values`,
#'   `residuals`, `convergence`, `lambda`, and `settings`.
#' @export
estimate_dl_from_quotes <- function(bonds,
                                    quotes,
                                    lambda = 0.0609,
                                    face_value = 100,
                                    accrued_day_count = "ACT/ACT",
                                    discount_day_count = "ACT/365.25",
                                    compounding = "annual",
                                    yields = NULL,
                                    weighting = c("none", "inverse_duration"),
                                    progress = FALSE) {
  validate_lambda(lambda)
  weighting <- match.arg(weighting)

  dirty_prices <- calculate_dirty_prices(
    bonds = bonds,
    quotes = quotes,
    face_value = face_value,
    day_count = accrued_day_count
  )

  fit <- fit_dl_dirty_prices(
    bonds = bonds,
    dirty_prices = dirty_prices,
    lambda = lambda,
    face_value = face_value,
    discount_day_count = discount_day_count,
    compounding = compounding,
    weighting = weighting,
    progress = progress
  )

  ytm <- yields_for_estimation(
    yields = yields,
    bonds = bonds,
    quotes = quotes,
    face_value = face_value,
    accrued_day_count = accrued_day_count,
    discount_day_count = discount_day_count,
    compounding = compounding,
    progress = progress
  )

  factor_input <- ytm[, c("date", "bond_id", "time_to_maturity", "ytm"), drop = FALSE]
  names(factor_input) <- c("date", "bond_id", "maturity", "yield")
  factor_input <- factor_input[
    is.finite(factor_input$maturity) & is.finite(factor_input$yield),
    ,
    drop = FALSE
  ]
  if (nrow(factor_input) == 0) {
    stop("No finite yield-to-maturity observations are available for Diebold-Li estimation.", call. = FALSE)
  }

  structure(
    list(
      dirty_prices = dirty_prices,
      ytm = ytm,
      factor_input = factor_input,
      factors = fit$coefficients,
      fitted_values = fit$fitted_values,
      residuals = fit$residuals,
      convergence = fit$convergence,
      lambda = lambda,
      settings = list(
        accrued_day_count = accrued_day_count,
        discount_day_count = discount_day_count,
        compounding = compounding,
        weighting = weighting
      )
    ),
    class = "yc_dl_estimate"
  )
}

fit_dl_dirty_prices <- function(bonds,
                                dirty_prices,
                                lambda = 0.0609,
                                start = NULL,
                                face_value = 100,
                                discount_day_count = "ACT/365.25",
                                compounding = "annual",
                                weighting = c("none", "inverse_duration"),
                                progress = FALSE) {
  validate_bonds(bonds)
  validate_dirty_prices(dirty_prices)
  validate_lambda(lambda)
  weighting <- match.arg(weighting)

  face_value <- normalize_bond_vector(face_value, bonds$bond_id, "face_value", positive = TRUE)
  names(face_value) <- bonds$bond_id

  price_data <- prepare_dirty_price_data(
    bonds = bonds,
    dirty_prices = dirty_prices,
    face_value = face_value
  )
  if (nrow(price_data) < 3) {
    stop("At least three dirty-price observations are required to fit Diebold-Li from prices.", call. = FALSE)
  }

  compounding <- match.arg(compounding, c("annual", "continuous"))
  dates <- sort(unique(price_data$date))

  fits <- vector("list", length(dates))
  start_sources <- character(length(dates))
  current_start <- start
  progress_bar <- create_progress_bar(progress, length(dates), "Estimating Diebold-Li prices")
  on.exit(close_progress_bar(progress_bar), add = TRUE)

  for (i in seq_along(dates)) {
    d <- dates[i]
    start_sources[i] <- if (i == 1) {
      if (is.null(start)) "default" else "user"
    } else {
      "previous_date"
    }
    fits[[i]] <- fit_dl_prices_one_date(
      price_data = price_data[price_data$date == d, , drop = FALSE],
      lambda = lambda,
      start = current_start,
      discount_day_count = discount_day_count,
      compounding = compounding,
      weighting = weighting
    )
    current_start <- unlist(fits[[i]]$coefficients[1, c("level", "slope", "curvature")])
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
      model = "Diebold-Li price fit",
      convergence = convergence,
      settings = list(
        lambda = lambda,
        discount_day_count = discount_day_count,
        compounding = compounding,
        weighting = weighting
      )
    ),
    class = "yc_dl_price"
  )
}

fit_dl_prices_one_date <- function(price_data, lambda, start, discount_day_count, compounding, weighting) {
  if (nrow(price_data) < 3) {
    stop(
      "At least three bond price observations are required for date ",
      price_data$date[1],
      ".",
      call. = FALSE
    )
  }

  cash_flow_data <- build_price_cash_flow_data(price_data, discount_day_count)

  ns_start <- default_ns_price_start(price_data, cash_flow_data, compounding)
  default_start <- c(
    level = unname(ns_start["beta0"]),
    slope = unname(ns_start["beta1"]),
    curvature = 0
  )
  if (is.null(start)) {
    starts <- list(default_start)
  } else {
    start <- validate_start(start, c("level", "slope", "curvature"))
    starts <- list(
      start,
      c(
      level = unname(ns_start["beta0"]),
      slope = unname(ns_start["beta1"]),
      curvature = 0
      )
    )
  }

  objective_weights <- price_objective_weights(
    price_data = price_data,
    cash_flow_data = cash_flow_data,
    weighting = weighting,
    compounding = compounding
  )
  objective <- function(par) {
    model_prices <- dl_model_prices_from_cash_flows(cash_flow_data, par, lambda, compounding)
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
    lower = c(level = -0.5, slope = -0.5, curvature = -0.5),
    upper = c(level = 0.5, slope = 0.5, curvature = 0.5),
    parscale = c(level = 0.01, slope = 0.01, curvature = 0.01)
  )

  coefficients <- stats::setNames(fit$par, c("level", "slope", "curvature"))
  model_prices <- dl_model_prices_from_cash_flows(cash_flow_data, coefficients, lambda, compounding)
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

dl_model_prices_from_cash_flows <- function(cash_flow_data, coefficients, lambda, compounding) {
  zero_rates <- as.numeric(dl_loadings(cash_flow_data$discount_time, lambda) %*% c(
    coefficients["level"],
    coefficients["slope"],
    coefficients["curvature"]
  ))
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

#' Plot Diebold-Li Term Structure
#'
#' Plots the estimated Diebold-Li yield curve for one quote date and overlays
#' observed bond-level yield-to-maturity values.
#'
#' @param x An object of class `"yc_dl_estimate"` or a data frame returned by
#'   [fit_dl_factors()].
#' @param date Quote date to plot. If omitted, the last available date is used.
#' @param lambda Positive fixed Diebold-Li decay parameter. Required when `x`
#'   is a factor data frame unless the default `0.0609` should be used.
#' @param observed_yields Optional data frame with columns `date`, `bond_id`,
#'   `maturity`, and `yield`. This is useful when plotting a factor data frame
#'   directly.
#' @param maturity_grid Optional numeric vector of maturities in years used to
#'   draw the smooth DL curve.
#' @param n_grid Number of grid points used when `maturity_grid` is `NULL`.
#' @param curve_col Color for the fitted DL term structure.
#' @param point_col Color for observed bond points.
#' @param lwd Line width for the fitted DL curve.
#' @param pch Plotting character for observed bond points.
#' @param ... Additional arguments passed to [graphics::plot()].
#'
#' @return Invisibly returns a data frame with the curve values and, as an
#'   attribute named `"observed"`, the observed points used in the plot.
#' @export
plot_dl_term_structure <- function(x,
                                   date = NULL,
                                   lambda = 0.0609,
                                   observed_yields = NULL,
                                   maturity_grid = NULL,
                                   n_grid = 200,
                                   curve_col = yc_plot_palette()$curve,
                                   point_col = yc_plot_palette()$point,
                                   lwd = 2,
                                   pch = 21,
                                   ...) {
  if (inherits(x, "yc_dl_estimate")) {
    factors <- x$factors
    lambda <- x$lambda
    if (is.null(observed_yields)) {
      observed_yields <- x$factor_input
    }
  } else if (inherits(x, "yc_dl_price")) {
    factors <- x$coefficients
    lambda <- x$settings$lambda
  } else if (is.data.frame(x)) {
    factors <- x
    validate_lambda(lambda)
  } else {
    stop("x must be a yc_dl_estimate object, a yc_dl_price object, or a Diebold-Li factor data frame.", call. = FALSE)
  }

  check_required_columns(factors, c("date", "level", "slope", "curvature"), "factors")
  plot_date <- resolve_plot_date(date, factors$date)
  factor_row <- factors[factors$date == plot_date, , drop = FALSE]
  if (nrow(factor_row) != 1) {
    stop("No Diebold-Li factors are available for the requested date.", call. = FALSE)
  }

  observed_points <- dl_observed_points_for_plot(observed_yields, plot_date)

  if (is.null(maturity_grid)) {
    max_maturity <- if (nrow(observed_points) > 0) {
      max(observed_points$maturity, na.rm = TRUE)
    } else {
      30
    }
    maturity_grid <- seq(0.001, max_maturity * 1.05, length.out = n_grid)
  } else {
    maturity_grid <- as.numeric(maturity_grid)
    check_numeric_vector(maturity_grid, "maturity_grid", positive = TRUE)
  }

  loadings <- dl_loadings(maturity_grid, lambda)
  curve <- data.frame(
    maturity = maturity_grid,
    yield = as.numeric(loadings %*% c(
      factor_row$level,
      factor_row$slope,
      factor_row$curvature
    ))
  )

  plot_y_range <- range(curve$yield, observed_points$yield, na.rm = TRUE)
  old_par <- setup_yc_plot_par()
  on.exit(graphics::par(old_par), add = TRUE)
  cols <- yc_plot_palette()
  plot_args <- yc_plot_args(list(
    type = "l",
    col = curve_col,
    lwd = max(lwd, 2.4),
    xlab = "Maturity (years)",
    ylab = "Yield",
    ylim = plot_y_range,
    main = paste("Diebold-Li term structure", format(plot_date)),
    bty = "n"
  ), ...)
  do.call(
    graphics::plot,
    c(list(x = curve$maturity, y = curve$yield), plot_args)
  )
  add_yc_grid()
  graphics::lines(curve$maturity, curve$yield, col = plot_args$col, lwd = plot_args$lwd)

  if (nrow(observed_points) > 0) {
    graphics::points(
      observed_points$maturity,
      observed_points$yield,
      col = point_col,
      bg = cols$point_fill,
      pch = pch,
      cex = 1.05
    )
  }

  attr(curve, "observed") <- observed_points
  invisible(curve)
}

dl_observed_points_for_plot <- function(observed_yields, plot_date) {
  if (is.null(observed_yields)) {
    return(data.frame(
      bond_id = character(),
      maturity = numeric(),
      yield = numeric()
    ))
  }

  required <- c("date", "bond_id", "maturity", "yield")
  check_required_columns(observed_yields, required, "observed_yields")
  observed_yields$date <- as.Date(observed_yields$date)

  points <- observed_yields[
    observed_yields$date == plot_date,
    required,
    drop = FALSE
  ]
  points <- points[is.finite(points$maturity) & is.finite(points$yield), , drop = FALSE]
  points[order(points$maturity), , drop = FALSE]
}
