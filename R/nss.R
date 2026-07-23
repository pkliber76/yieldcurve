#' Nelson-Siegel-Svensson Yield Curve
#'
#' Evaluates the Nelson-Siegel-Svensson yield curve at one or more maturities.
#'
#' @param maturity Numeric vector of maturities in years.
#' @param beta0 Level parameter.
#' @param beta1 Slope parameter.
#' @param beta2 First curvature parameter.
#' @param beta3 Second curvature parameter.
#' @param lambda1 Positive first decay parameter.
#' @param lambda2 Positive second decay parameter.
#'
#' @return A numeric vector of fitted yields.
#' @export
nss_yield <- function(maturity, beta0, beta1, beta2, beta3, lambda1, lambda2) {
  maturity <- as_maturity(maturity)
  validate_lambda(lambda1, "lambda1")
  validate_lambda(lambda2, "lambda2")

  beta0 +
    beta1 * ns_l1(maturity, lambda1) +
    beta2 * ns_l2(maturity, lambda1) +
    beta3 * ns_l2(maturity, lambda2)
}

#' Fit Nelson-Siegel-Svensson Model to Observed Yields
#'
#' Estimates Nelson-Siegel-Svensson parameters from observed yields using
#' `stats::optim()` with the `"L-BFGS-B"` method.
#'
#' @param maturity Numeric vector of maturities in years. Values must be
#'   strictly positive for fitting.
#' @param yield Numeric vector of observed yields expressed as decimals.
#' @param start Optional named numeric vector with starting values for `beta0`,
#'   `beta1`, `beta2`, `beta3`, `lambda1`, and `lambda2`.
#'
#' @return An S3 object of class `"yc_nss"`.
#' @export
fit_nss_yields <- function(maturity, yield, start = NULL) {
  maturity <- as.numeric(maturity)
  yield <- as.numeric(yield)
  check_numeric_vector(maturity, "maturity", positive = TRUE)
  check_numeric_vector(yield, "yield")
  if (length(maturity) != length(yield)) {
    stop("maturity and yield must have the same length.", call. = FALSE)
  }
  if (length(yield) < 6) {
    stop("At least six observations are required to fit Nelson-Siegel-Svensson.", call. = FALSE)
  }

  if (is.null(start)) {
    ord <- order(maturity)
    start <- c(
      beta0 = yield[ord][length(yield)],
      beta1 = yield[ord][1] - yield[ord][length(yield)],
      beta2 = 0,
      beta3 = 0,
      lambda1 = 0.5,
      lambda2 = 1.5
    )
  } else {
    start <- validate_start(
      start,
      c("beta0", "beta1", "beta2", "beta3", "lambda1", "lambda2")
    )
  }

  objective <- function(par) {
    fitted <- nss_yield(maturity, par[1], par[2], par[3], par[4], par[5], par[6])
    sum((yield - fitted)^2)
  }

  fit <- stats::optim(
    par = start,
    fn = objective,
    method = "L-BFGS-B",
    lower = c(
      beta0 = -1, beta1 = -1, beta2 = -1, beta3 = -1,
      lambda1 = 1e-04, lambda2 = 1e-04
    ),
    upper = c(
      beta0 = 1, beta1 = 1, beta2 = 1, beta3 = 1,
      lambda1 = 10, lambda2 = 10
    )
  )

  coefficients <- stats::setNames(
    fit$par,
    c("beta0", "beta1", "beta2", "beta3", "lambda1", "lambda2")
  )
  fitted <- nss_yield(
    maturity,
    coefficients["beta0"],
    coefficients["beta1"],
    coefficients["beta2"],
    coefficients["beta3"],
    coefficients["lambda1"],
    coefficients["lambda2"]
  )

  structure(
    list(
      coefficients = coefficients,
      fitted = fitted,
      residuals = yield - fitted,
      maturity = maturity,
      observed = yield,
      model = "Nelson-Siegel-Svensson",
      convergence = list(
        code = fit$convergence,
        message = fit$message,
        value = fit$value,
        counts = fit$counts
      )
    ),
    class = "yc_nss"
  )
}
