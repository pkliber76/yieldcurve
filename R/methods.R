#' @export
print.yc_ns <- function(x, ...) {
  cat(x$model, "yield curve fit\n", sep = "")
  cat("Coefficients:\n")
  print(x$coefficients)
  cat("Convergence code:", x$convergence$code, "\n")
  invisible(x)
}

#' @export
summary.yc_ns <- function(object, ...) {
  out <- list(
    model = object$model,
    coefficients = object$coefficients,
    sigma = sqrt(mean(object$residuals^2)),
    residual_summary = summary(object$residuals),
    convergence = object$convergence
  )
  class(out) <- "summary.yc_ns"
  out
}

#' @export
predict.yc_ns <- function(object, newdata = NULL, ...) {
  maturity <- if (is.null(newdata)) object$maturity else as.numeric(newdata)
  ns_yield(
    maturity,
    object$coefficients["beta0"],
    object$coefficients["beta1"],
    object$coefficients["beta2"],
    object$coefficients["lambda"]
  )
}

#' @export
plot.yc_ns <- function(x, ...) {
  ord <- order(x$maturity)
  cols <- yc_plot_palette()
  plot_args <- yc_plot_args(list(
    xlab = "Maturity (years)",
    ylab = "Yield",
    main = x$model,
    col = cols$point,
    bg = cols$point_fill,
    pch = 21,
    cex = 1.05,
    bty = "n"
  ), ...)

  observed <- data.frame(
    maturity = x$maturity,
    yield = x$observed,
    series = "Observed"
  )
  fitted <- data.frame(
    maturity = x$maturity[ord],
    yield = x$fitted[ord],
    series = "Fitted"
  )

  p <- ggplot2::ggplot() +
    ggplot2::geom_point(
      data = observed,
      ggplot2::aes(x = maturity, y = yield, color = series, fill = series),
      shape = plot_args$pch,
      size = plot_args$cex * 2.2,
      stroke = 0.8
    ) +
    ggplot2::geom_line(
      data = fitted,
      ggplot2::aes(x = maturity, y = yield, color = series),
      linewidth = 0.8
    ) +
    ggplot2::scale_color_manual(
      values = c(Observed = plot_args$col, Fitted = cols$curve),
      breaks = c("Observed", "Fitted"),
      name = NULL
    ) +
    ggplot2::scale_fill_manual(
      values = c(Observed = plot_args$bg, Fitted = cols$curve),
      breaks = c("Observed", "Fitted"),
      name = NULL
    ) +
    yc_ggplot_labels(plot_args) +
    yc_ggplot_theme()

  p <- yc_ggplot_limits(p, plot_args)
  print(p)
  p
}
#' @export
print.yc_nss <- function(x, ...) {
  cat(x$model, "yield curve fit\n", sep = "")
  cat("Coefficients:\n")
  print(x$coefficients)
  cat("Convergence code:", x$convergence$code, "\n")
  invisible(x)
}

#' @export
summary.yc_nss <- function(object, ...) {
  out <- list(
    model = object$model,
    coefficients = object$coefficients,
    sigma = sqrt(mean(object$residuals^2)),
    residual_summary = summary(object$residuals),
    convergence = object$convergence
  )
  class(out) <- "summary.yc_nss"
  out
}

#' @export
predict.yc_nss <- function(object, newdata = NULL, ...) {
  maturity <- if (is.null(newdata)) object$maturity else as.numeric(newdata)
  nss_yield(
    maturity,
    object$coefficients["beta0"],
    object$coefficients["beta1"],
    object$coefficients["beta2"],
    object$coefficients["beta3"],
    object$coefficients["lambda1"],
    object$coefficients["lambda2"]
  )
}

#' @export
plot.yc_nss <- function(x, ...) {
  ord <- order(x$maturity)
  cols <- yc_plot_palette()
  plot_args <- yc_plot_args(list(
    xlab = "Maturity (years)",
    ylab = "Yield",
    main = x$model,
    col = cols$point,
    bg = cols$point_fill,
    pch = 21,
    cex = 1.05,
    bty = "n"
  ), ...)

  observed <- data.frame(
    maturity = x$maturity,
    yield = x$observed,
    series = "Observed"
  )
  fitted <- data.frame(
    maturity = x$maturity[ord],
    yield = x$fitted[ord],
    series = "Fitted"
  )

  p <- ggplot2::ggplot() +
    ggplot2::geom_point(
      data = observed,
      ggplot2::aes(x = maturity, y = yield, color = series, fill = series),
      shape = plot_args$pch,
      size = plot_args$cex * 2.2,
      stroke = 0.8
    ) +
    ggplot2::geom_line(
      data = fitted,
      ggplot2::aes(x = maturity, y = yield, color = series),
      linewidth = 0.8
    ) +
    ggplot2::scale_color_manual(
      values = c(Observed = plot_args$col, Fitted = cols$curve),
      breaks = c("Observed", "Fitted"),
      name = NULL
    ) +
    ggplot2::scale_fill_manual(
      values = c(Observed = plot_args$bg, Fitted = cols$curve),
      breaks = c("Observed", "Fitted"),
      name = NULL
    ) +
    yc_ggplot_labels(plot_args) +
    yc_ggplot_theme()

  p <- yc_ggplot_limits(p, plot_args)
  print(p)
  p
}
#' Plot Price-Based NS Term Structure
#'
#' Plots the estimated Nelson-Siegel zero-rate term structure for one quote
#' date and overlays observed bond-level values used in the estimation. When
#' `x` is the object returned by [estimate_ns_from_quotes()], observed points
#' are taken from its yield-to-maturity diagnostics. When `x` is a
#' `"yc_ns_price"` object, pass a compatible `observed_yields` data frame if
#' observed points should be displayed.
#'
#' @param x An object of class `"yc_ns_estimate"` or `"yc_ns_price"`.
#' @param date Quote date to plot. If omitted, the last available date is used.
#' @param observed_yields Optional data frame with columns `date`, `bond_id`,
#'   `time_to_maturity`, and `ytm`. This is useful when plotting a
#'   `"yc_ns_price"` object directly.
#' @param maturity_grid Optional numeric vector of maturities in years used to
#'   draw the smooth NS curve.
#' @param n_grid Number of grid points used when `maturity_grid` is `NULL`.
#' @param curve_col Color for the fitted NS term structure.
#' @param point_col Color for observed bond points.
#' @param lwd Line width for the fitted NS curve.
#' @param pch Plotting character for observed bond points.
#' @param ... Additional named styling arguments such as `xlab`, `ylab`, `main`, `ylim`, `col`, or `lwd`.
#'
#' @return A `ggplot` object containing the term-structure plot. The curve data frame is stored in attribute `"curve"` and observed points in attribute `"observed"`. The plot is also printed.
#' @export
plot_ns_term_structure <- function(x,
                                   date = NULL,
                                   observed_yields = NULL,
                                   maturity_grid = NULL,
                                   n_grid = 200,
                                   curve_col = yc_plot_palette()$curve,
                                   point_col = yc_plot_palette()$point,
                                   lwd = 2,
                                   pch = 21,
                                   ...) {
  if (inherits(x, "yc_ns_estimate")) {
    fit <- x$fit
    if (is.null(observed_yields) && !is.null(x$ytm)) {
      observed_yields <- x$ytm
    }
  } else if (inherits(x, "yc_ns_price")) {
    fit <- x
  } else {
    stop("x must be a yc_ns_estimate or yc_ns_price object.", call. = FALSE)
  }

  plot_date <- resolve_plot_date(date, fit$coefficients$date)
  coefficient_row <- fit$coefficients[fit$coefficients$date == plot_date, , drop = FALSE]
  price_points <- fit$fitted_values[fit$fitted_values$date == plot_date, , drop = FALSE]

  if (nrow(coefficient_row) != 1 || nrow(price_points) == 0) {
    stop("No NS price fit is available for the requested date.", call. = FALSE)
  }

  observed_points <- observed_points_for_plot(
    observed_yields = observed_yields,
    price_points = price_points,
    plot_date = plot_date
  )

  if (is.null(maturity_grid)) {
    max_maturity <- max(price_points$time_to_maturity, observed_points$time_to_maturity, na.rm = TRUE)
    maturity_grid <- seq(0.001, max_maturity * 1.05, length.out = n_grid)
  } else {
    maturity_grid <- as.numeric(maturity_grid)
    check_numeric_vector(maturity_grid, "maturity_grid", positive = TRUE)
  }

  curve <- data.frame(
    maturity = maturity_grid,
    yield = ns_yield(
      maturity_grid,
      coefficient_row$beta0,
      coefficient_row$beta1,
      coefficient_row$beta2,
      coefficient_row$lambda
    )
  )

  plot_y_range <- range(curve$yield, observed_points$ytm, na.rm = TRUE)
  cols <- yc_plot_palette()
  plot_args <- yc_plot_args(list(
    col = curve_col,
    lwd = max(lwd, 2.4),
    xlab = "Maturity (years)",
    ylab = "Yield",
    ylim = plot_y_range,
    main = paste("NS term structure", format(plot_date)),
    bty = "n"
  ), ...)

  p <- yc_term_structure_plot(
    curve = curve,
    observed = observed_points,
    observed_x = "time_to_maturity",
    observed_y = "ytm",
    curve_col = plot_args$col,
    point_col = point_col,
    point_fill = cols$point_fill,
    lwd = plot_args$lwd,
    pch = pch,
    plot_args = plot_args
  )
  attr(p, "curve") <- curve
  attr(p, "observed") <- observed_points
  print(p)
  p
}
#' Plot Price-Based NSS Term Structure
#'
#' Plots the estimated Nelson-Siegel-Svensson zero-rate term structure for one
#' quote date and overlays observed bond-level values used in the estimation.
#' When `x` is the object returned by [estimate_nss_from_quotes()], observed
#' points are taken from its yield-to-maturity diagnostics. When `x` is a
#' `"yc_nss_price"` object, pass a compatible `observed_yields` data frame if
#' observed points should be displayed.
#'
#' @param x An object of class `"yc_nss_estimate"` or `"yc_nss_price"`.
#' @param date Quote date to plot. If omitted, the last available date is used.
#' @param observed_yields Optional data frame with columns `date`, `bond_id`,
#'   `time_to_maturity`, and `ytm`. This is useful when plotting a
#'   `"yc_nss_price"` object directly.
#' @param maturity_grid Optional numeric vector of maturities in years used to
#'   draw the smooth NSS curve.
#' @param n_grid Number of grid points used when `maturity_grid` is `NULL`.
#' @param curve_col Color for the fitted NSS term structure.
#' @param point_col Color for observed bond points.
#' @param lwd Line width for the fitted NSS curve.
#' @param pch Plotting character for observed bond points.
#' @param ... Additional named styling arguments such as `xlab`, `ylab`, `main`, `ylim`, `col`, or `lwd`.
#'
#' @return A `ggplot` object containing the term-structure plot. The curve data frame is stored in attribute `"curve"` and observed points in attribute `"observed"`. The plot is also printed.
#' @export
#'
#' @examples
#' bonds <- data.frame(
#'   bond_id = paste0("Z", 1:6),
#'   issue_date = as.Date(rep("2026-01-01", 6)),
#'   maturity = as.Date(c(
#'     "2026-07-01", "2027-01-01", "2028-01-01",
#'     "2030-01-01", "2033-01-01", "2036-01-01"
#'   )),
#'   coupon_rate = 0,
#'   coupon_frequency = 1L
#' )
#' time <- year_fraction(as.Date("2026-01-01"), bonds$maturity)
#' pars <- c(beta0 = 0.035, beta1 = -0.015, beta2 = 0.01,
#'   beta3 = 0.005, lambda1 = 0.6, lambda2 = 1.8)
#' rates <- nss_yield(time, pars[1], pars[2], pars[3], pars[4], pars[5], pars[6])
#' quotes <- data.frame(
#'   date = as.Date("2026-01-01"),
#'   stats::setNames(as.list(100 * (1 + rates)^(-time)), bonds$bond_id)
#' )
#' fit <- estimate_nss_from_quotes(bonds, quotes, start = pars)
#' plot_nss_term_structure(fit, as.Date("2026-01-01"))
plot_nss_term_structure <- function(x,
                                    date = NULL,
                                    observed_yields = NULL,
                                    maturity_grid = NULL,
                                    n_grid = 200,
                                    curve_col = yc_plot_palette()$curve,
                                    point_col = yc_plot_palette()$point,
                                    lwd = 2,
                                    pch = 21,
                                    ...) {
  if (inherits(x, "yc_nss_estimate")) {
    fit <- x$fit
    if (is.null(observed_yields) && !is.null(x$ytm)) {
      observed_yields <- x$ytm
    }
  } else if (inherits(x, "yc_nss_price")) {
    fit <- x
  } else {
    stop("x must be a yc_nss_estimate or yc_nss_price object.", call. = FALSE)
  }

  plot_date <- resolve_plot_date(date, fit$coefficients$date)
  coefficient_row <- fit$coefficients[fit$coefficients$date == plot_date, , drop = FALSE]
  price_points <- fit$fitted_values[fit$fitted_values$date == plot_date, , drop = FALSE]

  if (nrow(coefficient_row) != 1 || nrow(price_points) == 0) {
    stop("No NSS price fit is available for the requested date.", call. = FALSE)
  }

  observed_points <- observed_points_for_plot(
    observed_yields = observed_yields,
    price_points = price_points,
    plot_date = plot_date
  )

  if (is.null(maturity_grid)) {
    max_maturity <- max(price_points$time_to_maturity, observed_points$time_to_maturity, na.rm = TRUE)
    maturity_grid <- seq(0.001, max_maturity * 1.05, length.out = n_grid)
  } else {
    maturity_grid <- as.numeric(maturity_grid)
    check_numeric_vector(maturity_grid, "maturity_grid", positive = TRUE)
  }

  curve <- data.frame(
    maturity = maturity_grid,
    yield = nss_yield(
      maturity_grid,
      coefficient_row$beta0,
      coefficient_row$beta1,
      coefficient_row$beta2,
      coefficient_row$beta3,
      coefficient_row$lambda1,
      coefficient_row$lambda2
    )
  )

  plot_y_range <- range(curve$yield, observed_points$ytm, na.rm = TRUE)
  cols <- yc_plot_palette()
  plot_args <- yc_plot_args(list(
    col = curve_col,
    lwd = max(lwd, 2.4),
    xlab = "Maturity (years)",
    ylab = "Yield",
    ylim = plot_y_range,
    main = paste("NSS term structure", format(plot_date)),
    bty = "n"
  ), ...)

  p <- yc_term_structure_plot(
    curve = curve,
    observed = observed_points,
    observed_x = "time_to_maturity",
    observed_y = "ytm",
    curve_col = plot_args$col,
    point_col = point_col,
    point_fill = cols$point_fill,
    lwd = plot_args$lwd,
    pch = pch,
    plot_args = plot_args
  )
  attr(p, "curve") <- curve
  attr(p, "observed") <- observed_points
  print(p)
  p
}
resolve_plot_date <- function(date, available_dates) {
  if (is.null(date)) {
    return(max(as.Date(available_dates)))
  }

  date <- as.Date(date)
  if (length(date) != 1 || is.na(date)) {
    stop("date must be a single valid Date or date-like value.", call. = FALSE)
  }
  if (!date %in% available_dates) {
    stop("Requested date is not present in the price fit.", call. = FALSE)
  }
  date
}

observed_points_for_plot <- function(observed_yields, price_points, plot_date) {
  if (is.null(observed_yields)) {
    return(data.frame(
      bond_id = character(),
      time_to_maturity = numeric(),
      ytm = numeric()
    ))
  }

  required <- c("date", "bond_id", "time_to_maturity", "ytm")
  check_required_columns(observed_yields, required, "observed_yields")
  observed_yields$date <- as.Date(observed_yields$date)

  points <- observed_yields[
    observed_yields$date == plot_date & observed_yields$bond_id %in% price_points$bond_id,
    required,
    drop = FALSE
  ]
  points <- points[is.finite(points$time_to_maturity) & is.finite(points$ytm), , drop = FALSE]
  points[order(points$time_to_maturity), , drop = FALSE]
}

#' @export
print.summary.yc_ns <- function(x, ...) {
  cat(x$model, "yield curve fit summary\n", sep = "")
  cat("Coefficients:\n")
  print(x$coefficients)
  cat("Residual RMSE:", x$sigma, "\n")
  cat("Residuals:\n")
  print(x$residual_summary)
  cat("Convergence code:", x$convergence$code, "\n")
  invisible(x)
}

#' @export
print.summary.yc_nss <- function(x, ...) {
  cat(x$model, "yield curve fit summary\n", sep = "")
  cat("Coefficients:\n")
  print(x$coefficients)
  cat("Residual RMSE:", x$sigma, "\n")
  cat("Residuals:\n")
  print(x$residual_summary)
  cat("Convergence code:", x$convergence$code, "\n")
  invisible(x)
}

#' @export
print.yc_ns_price <- function(x, ...) {
  cat(x$model, "\n", sep = "")
  cat("Coefficient rows:", nrow(x$coefficients), "\n")
  cat("Price observations:", nrow(x$fitted_values), "\n")
  cat("Convergence codes:\n")
  print(x$convergence[, c("date", "code")])
  invisible(x)
}

#' Plot Price-Based Nelson-Siegel Results
#'
#' Plots a `"yc_ns_price"` object returned by [fit_ns_dirty_prices()] or
#' [fit_ns_prices()]. This is a generic `plot()` method that draws the
#' estimated Nelson-Siegel term structure for one quote date. Observed
#' yield-to-maturity points can be supplied with `observed_yields`.
#'
#' @param x An object of class `"yc_ns_price"`.
#' @param type Plot type. Currently only `"term_structure"` is supported.
#' @param date Quote date to plot. If omitted, the last available date is used.
#' @param observed_yields Optional data frame with columns `date`, `bond_id`,
#'   `time_to_maturity`, and `ytm`.
#' @param ... Additional arguments passed to [plot_ns_term_structure()].
#'
#' @return A `ggplot` object returned by [plot_ns_term_structure()]. The plot is also printed.
#' @export
plot.yc_ns_price <- function(x,
                             date = NULL,
                             type = c("term_structure"),
                             observed_yields = NULL,
                             ...) {
  type <- match.arg(type)
  plot_ns_term_structure(
    x = x,
    date = date,
    observed_yields = observed_yields,
    ...
  )
}

#' @export
summary.yc_ns_price <- function(object, ...) {
  out <- list(
    model = object$model,
    coefficients = object$coefficients,
    price_rmse = sqrt(mean(object$fitted_values$price_residual^2)),
    residual_summary = summary(object$fitted_values$price_residual),
    convergence = object$convergence,
    settings = object$settings
  )
  class(out) <- "summary.yc_ns_price"
  out
}

#' @export
print.summary.yc_ns_price <- function(x, ...) {
  cat(x$model, " summary\n", sep = "")
  cat("Coefficients:\n")
  print(x$coefficients)
  cat("Price RMSE:", x$price_rmse, "\n")
  cat("Price residuals:\n")
  print(x$residual_summary)
  cat("Convergence:\n")
  print(x$convergence)
  invisible(x)
}

#' @export
print.yc_nss_price <- function(x, ...) {
  cat(x$model, "\n", sep = "")
  cat("Coefficient rows:", nrow(x$coefficients), "\n")
  cat("Price observations:", nrow(x$fitted_values), "\n")
  cat("Convergence codes:\n")
  print(x$convergence[, c("date", "code")])
  invisible(x)
}

#' Plot Price-Based Nelson-Siegel-Svensson Results
#'
#' Plots a `"yc_nss_price"` object returned by [fit_nss_dirty_prices()] or
#' [fit_nss_prices()]. This is a generic `plot()` method that draws the
#' estimated Nelson-Siegel-Svensson term structure for one quote date. Observed
#' yield-to-maturity points can be supplied with `observed_yields`.
#'
#' @param x An object of class `"yc_nss_price"`.
#' @param type Plot type. Currently only `"term_structure"` is supported.
#' @param date Quote date to plot. If omitted, the last available date is used.
#' @param observed_yields Optional data frame with columns `date`, `bond_id`,
#'   `time_to_maturity`, and `ytm`.
#' @param ... Additional arguments passed to [plot_nss_term_structure()].
#'
#' @return A `ggplot` object returned by [plot_nss_term_structure()]. The plot is also printed.
#' @export
plot.yc_nss_price <- function(x,
                              date = NULL,
                              type = c("term_structure"),
                              observed_yields = NULL,
                              ...) {
  type <- match.arg(type)
  plot_nss_term_structure(
    x = x,
    date = date,
    observed_yields = observed_yields,
    ...
  )
}

#' @export
print.yc_ns_estimate <- function(x, ...) {
  cat("NS estimation\n")
  cat("Dirty price dates:", count_price_dates(x$dirty_prices), "\n")
  cat("NS coefficient rows:", nrow(x$fit$coefficients), "\n")
  if (!is.null(x$ytm)) {
    cat("YTM observations:", nrow(x$ytm), "\n")
  }
  invisible(x)
}

#' Plot Nelson-Siegel Estimation Results
#'
#' Plots results from [estimate_ns_from_quotes()]. Currently this draws the
#' estimated Nelson-Siegel term structure for one quote date together with the
#' observed bond yield-to-maturity points used in estimation.
#'
#' @param x An object of class `"yc_ns_estimate"`.
#' @param type Plot type. Currently only `"term_structure"` is supported.
#' @param date Quote date to plot. If omitted, the last available date is used.
#' @param ... Additional arguments passed to [plot_ns_term_structure()].
#'
#' @return A `ggplot` object returned by [plot_ns_term_structure()]. The plot is also printed.
#' @export
plot.yc_ns_estimate <- function(x,
                                date = NULL,
                                type = c("term_structure"),
                                ...) {
  type <- match.arg(type)
  plot_ns_term_structure(
    x = x,
    date = date,
    ...
  )
}

#' @export
print.yc_dl_estimate <- function(x, ...) {
  cat("Diebold-Li estimation\n")
  cat("Lambda:", x$lambda, "\n")
  cat("Dirty price dates:", count_price_dates(x$dirty_prices), "\n")
  cat("Factor rows:", nrow(x$factors), "\n")
  cat("YTM observations:", nrow(x$ytm), "\n")
  invisible(x)
}

#' Plot Diebold-Li Estimation Results
#'
#' Plots results from [estimate_dl_from_quotes()]. By default it plots the
#' level, slope, and curvature factor time series. Use `type = "term_structure"`
#' to plot the estimated Diebold-Li curve for a specific date together with the
#' observed bond yield-to-maturity points used in factor estimation.
#'
#' @param x An object of class `"yc_dl_estimate"`.
#' @param type Plot type. `"factors"` plots the factor time series;
#'   `"term_structure"` plots the yield curve for one date.
#' @param date Quote date used when `type = "term_structure"`. If omitted, the
#'   last available date is used.
#' @param ... Additional arguments passed to [plot_dl_factors()] or
#'   [plot_dl_term_structure()].
#'
#' @return A `ggplot` object containing the requested Diebold-Li plot. The plot is also printed.
#' @export
plot.yc_dl_estimate <- function(x,
                                date = NULL,
                                type = c("factors", "term_structure"),
                                ...) {
  type <- match.arg(type)

  if (type == "factors") {
    return(plot_dl_factors(x$factors, ...))
  }

  plot_dl_term_structure(
    x = x,
    date = date,
    ...
  )
}

#' Plot Price-Based Diebold-Li Results
#'
#' Plots a `"yc_dl_price"` object returned by the price-based Diebold-Li
#' fitting routine. Currently this draws the estimated Diebold-Li term
#' structure for one quote date.
#'
#' @param x An object of class `"yc_dl_price"`.
#' @param type Plot type. Currently only `"term_structure"` is supported.
#' @param date Quote date to plot. If omitted, the last available date is used.
#' @param observed_yields Optional data frame with columns `date`, `bond_id`,
#'   `maturity`, and `yield`.
#' @param ... Additional arguments passed to [plot_dl_term_structure()].
#'
#' @return A `ggplot` object returned by [plot_dl_term_structure()]. The plot is also printed.
#' @export
plot.yc_dl_price <- function(x,
                             date = NULL,
                             type = c("term_structure"),
                             observed_yields = NULL,
                             ...) {
  type <- match.arg(type)
  plot_dl_term_structure(
    x = x,
    date = date,
    observed_yields = observed_yields,
    ...
  )
}

#' @export
summary.yc_nss_price <- function(object, ...) {
  out <- list(
    model = object$model,
    coefficients = object$coefficients,
    price_rmse = sqrt(mean(object$fitted_values$price_residual^2)),
    residual_summary = summary(object$fitted_values$price_residual),
    convergence = object$convergence,
    settings = object$settings
  )
  class(out) <- "summary.yc_nss_price"
  out
}

#' @export
print.summary.yc_nss_price <- function(x, ...) {
  cat(x$model, " summary\n", sep = "")
  cat("Coefficients:\n")
  print(x$coefficients)
  cat("Price RMSE:", x$price_rmse, "\n")
  cat("Price residuals:\n")
  print(x$residual_summary)
  cat("Convergence:\n")
  print(x$convergence)
  invisible(x)
}

#' @export
print.yc_nss_estimate <- function(x, ...) {
  cat("NSS estimation\n")
  cat("Dirty price dates:", count_price_dates(x$dirty_prices), "\n")
  cat("NSS coefficient rows:", nrow(x$fit$coefficients), "\n")
  if (!is.null(x$ytm)) {
    cat("YTM observations:", nrow(x$ytm), "\n")
  }
  invisible(x)
}

#' Plot Nelson-Siegel-Svensson Estimation Results
#'
#' Plots results from [estimate_nss_from_quotes()]. Currently this draws the
#' estimated Nelson-Siegel-Svensson term structure for one quote date together
#' with the observed bond yield-to-maturity points used in estimation.
#'
#' @param x An object of class `"yc_nss_estimate"`.
#' @param type Plot type. Currently only `"term_structure"` is supported.
#' @param date Quote date to plot. If omitted, the last available date is used.
#' @param ... Additional arguments passed to [plot_nss_term_structure()].
#'
#' @return A `ggplot` object returned by [plot_nss_term_structure()]. The plot is also printed.
#' @export
plot.yc_nss_estimate <- function(x,
                                 date = NULL,
                                 type = c("term_structure"),
                                 ...) {
  type <- match.arg(type)
  plot_nss_term_structure(
    x = x,
    date = date,
    ...
  )
}

yc_plot_palette <- function() {
  list(
    curve = "#2563EB",
    curve_2 = "#0F766E",
    curve_3 = "#B45309",
    point = "#111827",
    point_fill = "#16F016",
    grid = "#E5E7EB",
    axis = "#374151"
  )
}

setup_yc_plot_par <- function() {
  graphics::par(no.readonly = TRUE)
}

add_yc_grid <- function() {
  invisible(NULL)
}

add_yc_legend <- function(legend, col, lty, pch = NA, lwd = NA, pt.bg = NA) {
  invisible(NULL)
}

yc_plot_args <- function(defaults, ...) {
  dots <- list(...)
  if (length(dots) == 0) {
    return(defaults)
  }

  if (is.null(names(dots)) || any(!nzchar(names(dots)))) {
    stop("Additional plot arguments must be named.", call. = FALSE)
  }

  for (nm in names(dots)) {
    defaults[[nm]] <- dots[[nm]]
  }
  defaults
}

yc_ggplot_theme <- function() {
  cols <- yc_plot_palette()
  ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_line(color = cols$grid, linewidth = 0.35),
      panel.grid.minor = ggplot2::element_blank(),
      axis.title = ggplot2::element_text(color = cols$axis),
      axis.text = ggplot2::element_text(color = cols$axis),
      plot.title = ggplot2::element_text(color = cols$axis, face = "bold"),
      legend.position = "right",
      legend.title = ggplot2::element_blank()
    )
}

yc_ggplot_labels <- function(plot_args) {
  ggplot2::labs(
    x = plot_args$xlab %||% NULL,
    y = plot_args$ylab %||% NULL,
    title = plot_args$main %||% NULL
  )
}

yc_ggplot_limits <- function(p, plot_args) {
  if (!is.null(plot_args$ylim)) {
    p <- p + ggplot2::coord_cartesian(ylim = plot_args$ylim)
  }
  p
}

yc_term_structure_plot <- function(curve,
                                   observed,
                                   observed_x,
                                   observed_y,
                                   curve_col,
                                   point_col,
                                   point_fill,
                                   lwd,
                                   pch,
                                   plot_args) {
  curve$series <- "Fitted"
  p <- ggplot2::ggplot(curve, ggplot2::aes(x = maturity, y = yield)) +
    ggplot2::geom_line(
      ggplot2::aes(color = series),
      linewidth = lwd / 3
    )

  if (nrow(observed) > 0) {
    observed_plot <- data.frame(
      maturity = observed[[observed_x]],
      yield = observed[[observed_y]],
      series = "Observed"
    )
    p <- p + ggplot2::geom_point(
      data = observed_plot,
      ggplot2::aes(x = maturity, y = yield, color = series, fill = series),
      shape = pch,
      size = 2.4,
      stroke = 0.8
    )
  }

  p <- p +
    ggplot2::scale_color_manual(
      values = c(Fitted = curve_col, Observed = point_col),
      breaks = c("Observed", "Fitted"),
      name = NULL
    ) +
    yc_ggplot_labels(plot_args) +
    yc_ggplot_theme()

  if (nrow(observed) > 0) {
    p <- p + ggplot2::scale_fill_manual(
      values = c(Observed = point_fill),
      breaks = "Observed",
      name = NULL
    )
  }

  yc_ggplot_limits(p, plot_args)
}

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

count_price_dates <- function(prices) {
  if (is.data.frame(prices) && "date" %in% names(prices)) {
    return(length(unique(prices$date)))
  }
  length(prices)
}