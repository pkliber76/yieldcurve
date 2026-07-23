#' Nelson-Siegel Yield Curve
#'
#' Evaluates the Nelson-Siegel yield curve at one or more maturities.
#'
#' @param maturity Numeric vector of maturities in years.
#' @param beta0 Level parameter.
#' @param beta1 Slope parameter.
#' @param beta2 Curvature parameter.
#' @param lambda Positive decay parameter.
#'
#' @return A numeric vector of fitted yields.
#' @export
ns_yield <- function(maturity, beta0, beta1, beta2, lambda) {
  maturity <- as_maturity(maturity)
  validate_lambda(lambda)

  beta0 + beta1 * ns_l1(maturity, lambda) + beta2 * ns_l2(maturity, lambda)
}

#' Fit Nelson-Siegel Model to Observed Yields
#'
#' Estimates Nelson-Siegel parameters from observed yields using
#' `stats::optim()` with the `"L-BFGS-B"` method.
#'
#' @param maturity Numeric vector of maturities in years. Values must be
#'   strictly positive for fitting.
#' @param yield Numeric vector of observed yields expressed as decimals.
#' @param start Optional named numeric vector with starting values for `beta0`,
#'   `beta1`, `beta2`, and `lambda`.
#'
#' @return An S3 object of class `"yc_ns"`.
#' @export
fit_ns_yields <- function(maturity, yield, start = NULL) {
  maturity <- as.numeric(maturity)
  yield <- as.numeric(yield)
  check_numeric_vector(maturity, "maturity", positive = TRUE)
  check_numeric_vector(yield, "yield")
  if (length(maturity) != length(yield)) {
    stop("maturity and yield must have the same length.", call. = FALSE)
  }
  if (length(yield) < 4) {
    stop("At least four observations are required to fit Nelson-Siegel.", call. = FALSE)
  }

  if (is.null(start)) {
    ord <- order(maturity)
    start <- c(
      beta0 = yield[ord][length(yield)],
      beta1 = yield[ord][1] - yield[ord][length(yield)],
      beta2 = 0,
      lambda = 0.5
    )
  } else {
    start <- validate_start(start, c("beta0", "beta1", "beta2", "lambda"))
  }

  objective <- function(par) {
    fitted <- ns_yield(maturity, par[1], par[2], par[3], par[4])
    sum((yield - fitted)^2)
  }

  fit <- stats::optim(
    par = start,
    fn = objective,
    method = "L-BFGS-B",
    lower = c(beta0 = -1, beta1 = -1, beta2 = -1, lambda = 1e-04),
    upper = c(beta0 = 1, beta1 = 1, beta2 = 1, lambda = 10)
  )

  coefficients <- stats::setNames(fit$par, c("beta0", "beta1", "beta2", "lambda"))
  fitted <- ns_yield(
    maturity,
    coefficients["beta0"],
    coefficients["beta1"],
    coefficients["beta2"],
    coefficients["lambda"]
  )

  structure(
    list(
      coefficients = coefficients,
      fitted = fitted,
      residuals = yield - fitted,
      maturity = maturity,
      observed = yield,
      model = "Nelson-Siegel",
      convergence = list(
        code = fit$convergence,
        message = fit$message,
        value = fit$value,
        counts = fit$counts
      )
    ),
    class = "yc_ns"
  )
}

validate_start <- function(start, required) {
  if (!is.numeric(start) || is.null(names(start))) {
    stop("start must be a named numeric vector.", call. = FALSE)
  }
  missing_names <- setdiff(required, names(start))
  if (length(missing_names) > 0) {
    stop(
      "start is missing required value(s): ",
      paste(missing_names, collapse = ", "),
      call. = FALSE
    )
  }
  start <- start[required]
  check_numeric_vector(start, "start")
  start
}
