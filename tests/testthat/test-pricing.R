make_price_panel <- function(date, bond_id, prices) {
  date <- as.Date(date)
  if (is.null(dim(prices))) {
    prices <- matrix(prices, nrow = length(date), byrow = TRUE)
  }
  out <- data.frame(date = date)
  for (i in seq_along(bond_id)) {
    out[[bond_id[i]]] <- prices[, i]
  }
  out
}

test_that("year_fraction computes ACT/365.25 timing", {
  expect_equal(
    year_fraction(as.Date("2026-01-01"), as.Date("2027-01-01")),
    365 / 365.25
  )
})

test_that("calculate_dirty_prices adds accrued interest to clean prices", {
  bonds <- data.frame(
    bond_id = "B1",
    issue_date = as.Date("2026-01-01"),
    maturity = as.Date("2027-01-01"),
    coupon_rate = 0.04,
    coupon_frequency = 2L
  )

  quotes <- data.frame(
    date = as.Date("2026-04-01"),
    B1 = 99
  )

  prices <- calculate_dirty_prices(bonds, quotes, face_value = 100)
  prices_df <- calculate_dirty_price_data_frame(bonds, quotes, face_value = 100)

  expected_accrued <- 2 * as.numeric(as.Date("2026-04-01") - as.Date("2026-01-01")) /
    as.numeric(as.Date("2026-07-01") - as.Date("2026-01-01"))
  expect_s3_class(prices, "data.frame")
  expect_named(prices, c("date", "B1"))
  expect_equal(prices$date, as.Date("2026-04-01"))
  expect_equal(prices$B1, 99 + expected_accrued, tolerance = 1e-10)
  expect_equal(prices_df$accrued_interest, expected_accrued, tolerance = 1e-10)
  expect_equal(prices_df$dirty_price, 99 + expected_accrued, tolerance = 1e-10)
})

test_that("calculate_dirty_prices accepts named face values", {
  bonds <- data.frame(
    bond_id = c("B1", "B2"),
    issue_date = as.Date(c("2026-01-01", "2026-01-01")),
    maturity = as.Date(c("2027-01-01", "2028-01-01")),
    coupon_rate = c(0.04, 0.05),
    coupon_frequency = c(2L, 1L)
  )

  quotes <- data.frame(
    date = as.Date("2026-04-01"),
    B1 = 99,
    B2 = 101
  )

  prices <- calculate_dirty_price_data_frame(
    bonds = bonds,
    quotes = quotes,
    face_value = c(B2 = 1000, B1 = 100)
  )

  expect_equal(prices$face_value, c(100, 1000))
  expect_true(all(prices$dirty_price > 0))
})

test_that("calculate_dirty_prices has zero accrued interest on coupon dates", {
  bonds <- data.frame(
    bond_id = "B1",
    issue_date = as.Date("2026-01-01"),
    maturity = as.Date("2027-01-01"),
    coupon_rate = 0.04,
    coupon_frequency = 2L
  )
  quotes <- data.frame(
    date = as.Date("2026-07-01"),
    B1 = 100
  )

  prices <- calculate_dirty_prices(bonds, quotes)

  expect_equal(prices$B1, 100)
})

test_that("calculate_dirty_prices supports multiple quote dates", {
  bonds <- data.frame(
    bond_id = "B1",
    issue_date = as.Date("2026-01-01"),
    maturity = as.Date("2027-01-01"),
    coupon_rate = 0.04,
    coupon_frequency = 2L
  )
  quotes <- data.frame(
    date = as.Date(c("2026-04-01", "2026-05-01")),
    B1 = c(99, 100)
  )

  prices <- calculate_dirty_prices(bonds, quotes)
  prices_df <- calculate_dirty_price_data_frame(bonds, quotes)

  expect_s3_class(prices, "data.frame")
  expect_equal(prices$date, as.Date(c("2026-04-01", "2026-05-01")))
  expect_equal(nrow(prices_df), 2)
  expect_equal(prices_df$dirty_price, prices_df$clean_price + prices_df$accrued_interest)
})

test_that("calculate_clean_prices subtracts accrued interest from dirty prices", {
  bonds <- data.frame(
    bond_id = "B1",
    issue_date = as.Date("2026-01-01"),
    maturity = as.Date("2027-01-01"),
    coupon_rate = 0.04,
    coupon_frequency = 2L
  )
  quotes <- data.frame(
    date = as.Date("2026-04-01"),
    B1 = 99
  )
  dirty_prices <- calculate_dirty_prices(bonds, quotes, face_value = 100)
  clean_prices <- calculate_clean_prices(bonds, dirty_prices, face_value = 100)

  expect_s3_class(clean_prices, "data.frame")
  expect_equal(clean_prices$date, quotes$date)
  expect_named(clean_prices, c("date", "B1"))
  expect_equal(clean_prices$B1, 99, tolerance = 1e-10)
})

test_that("calculate_ytm recovers a zero-coupon yield", {
  bonds <- data.frame(
    bond_id = "Z1",
    issue_date = as.Date("2026-01-01"),
    maturity = as.Date("2027-01-01"),
    coupon_rate = 0,
    coupon_frequency = 1L
  )
  clean_price <- 100 / 1.05
  quotes <- data.frame(
    date = as.Date("2026-01-01"),
    Z1 = clean_price
  )

  ytm <- calculate_ytm(
    bonds = bonds,
    quotes = quotes,
    face_value = 100,
    discount_day_count = "ACT/365",
    compounding = "annual"
  )

  expect_equal(ytm$ytm, 0.05, tolerance = 1e-10)
})

test_that("fit_ns_prices returns price-based NS fit", {
  settlement_date <- as.Date("2026-01-01")
  maturities <- as.Date(c(
    "2026-07-01", "2027-01-01", "2028-01-01",
    "2030-01-01", "2033-01-01", "2036-01-01"
  ))
  bonds <- data.frame(
    bond_id = paste0("Z", seq_along(maturities)),
    issue_date = rep(settlement_date, length(maturities)),
    maturity = maturities,
    coupon_rate = 0,
    coupon_frequency = 1L
  )
  time <- year_fraction(settlement_date, maturities, "ACT/365.25")
  pars <- c(beta0 = 0.035, beta1 = -0.015, beta2 = 0.01, lambda = 0.6)
  zero_rates <- ns_yield(time, pars[1], pars[2], pars[3], pars[4])
  clean_prices <- 100 * (1 + zero_rates)^(-time)
  quotes <- make_price_panel(settlement_date, bonds$bond_id, clean_prices)

  fit <- fit_ns_prices(
    bonds = bonds,
    quotes = quotes,
    face_value = 100,
    start = pars
  )

  expect_s3_class(fit, "yc_ns_price")
  expect_named(fit$coefficients, c("date", "beta0", "beta1", "beta2", "lambda"))
  expect_equal(fit$convergence$code, 0)
  expect_lt(max(abs(fit$fitted_values$price_residual)), 1e-6)
})

test_that("estimate_ns_from_quotes returns estimation outputs", {
  settlement_date <- as.Date("2026-01-01")
  maturities <- as.Date(c(
    "2026-07-01", "2027-01-01", "2028-01-01",
    "2030-01-01", "2033-01-01", "2036-01-01"
  ))
  bonds <- data.frame(
    bond_id = paste0("Z", seq_along(maturities)),
    issue_date = rep(settlement_date, length(maturities)),
    maturity = maturities,
    coupon_rate = 0,
    coupon_frequency = 1L
  )
  time <- year_fraction(settlement_date, maturities, "ACT/365.25")
  pars <- c(beta0 = 0.035, beta1 = -0.015, beta2 = 0.01, lambda = 0.6)
  zero_rates <- ns_yield(time, pars[1], pars[2], pars[3], pars[4])
  clean_prices <- 100 * (1 + zero_rates)^(-time)
  quotes <- make_price_panel(settlement_date, bonds$bond_id, clean_prices)

  est <- estimate_ns_from_quotes(
    bonds = bonds,
    quotes = quotes,
    face_value = 100,
    start = pars
  )

  expect_s3_class(est, "yc_ns_estimate")
  expect_equal(nrow(est$dirty_prices), 1)
  expect_s3_class(est$fit, "yc_ns_price")
  expect_equal(nrow(est$ytm), 6)
})

test_that("estimate_dl_from_quotes returns factors with default and custom lambda", {
  settlement_date <- as.Date("2026-01-01")
  maturities <- as.Date(c(
    "2026-07-01", "2027-01-01", "2028-01-01",
    "2030-01-01", "2033-01-01", "2036-01-01"
  ))
  bonds <- data.frame(
    bond_id = paste0("Z", seq_along(maturities)),
    issue_date = rep(settlement_date, length(maturities)),
    maturity = maturities,
    coupon_rate = 0,
    coupon_frequency = 1L
  )
  time <- year_fraction(settlement_date, maturities, "ACT/365")
  observed_yields <- c(0.031, 0.033, 0.035, 0.038, 0.041, 0.043)
  clean_prices <- 100 * (1 + observed_yields)^(-time)
  quotes <- make_price_panel(settlement_date, bonds$bond_id, clean_prices)

  default_estimate <- estimate_dl_from_quotes(
    bonds = bonds,
    quotes = quotes,
    face_value = 100,
    discount_day_count = "ACT/365"
  )
  custom_estimate <- estimate_dl_from_quotes(
    bonds = bonds,
    quotes = quotes,
    lambda = 0.1,
    face_value = 100,
    discount_day_count = "ACT/365"
  )

  expect_s3_class(default_estimate, "yc_dl_estimate")
  expect_equal(default_estimate$lambda, 0.0609)
  expect_equal(custom_estimate$lambda, 0.1)
  expect_named(default_estimate$factors, c("date", "level", "slope", "curvature"))
  expect_equal(nrow(default_estimate$factors), 1)
  expect_named(
    default_estimate$convergence,
    c("date", "code", "value", "start_source")
  )
  expect_true(all(c("fitted_dirty_price", "price_residual") %in% names(default_estimate$fitted_values)))
})

test_that("price-based estimates support inverse-duration objective weights", {
  settlement_date <- as.Date("2026-01-01")
  maturities <- as.Date(c(
    "2026-07-01", "2027-01-01", "2028-01-01",
    "2030-01-01", "2033-01-01", "2036-01-01"
  ))
  bonds <- data.frame(
    bond_id = paste0("Z", seq_along(maturities)),
    issue_date = rep(settlement_date, length(maturities)),
    maturity = maturities,
    coupon_rate = 0,
    coupon_frequency = 1L
  )
  time <- year_fraction(settlement_date, maturities, "ACT/365.25")
  ns_pars <- c(beta0 = 0.035, beta1 = -0.015, beta2 = 0.01, lambda = 0.6)
  zero_rates <- ns_yield(time, ns_pars[1], ns_pars[2], ns_pars[3], ns_pars[4])
  clean_prices <- 100 * (1 + zero_rates)^(-time)
  quotes <- make_price_panel(settlement_date, bonds$bond_id, clean_prices)

  ns_estimate <- estimate_ns_from_quotes(
    bonds = bonds,
    quotes = quotes,
    start = ns_pars,
    include_ytm = FALSE,
    weighting = "inverse_duration"
  )
  nss_estimate <- estimate_nss_from_quotes(
    bonds = bonds,
    quotes = quotes,
    start = c(beta0 = 0.035, beta1 = -0.015, beta2 = 0.01, beta3 = 0, lambda1 = 0.6, lambda2 = 1.8),
    include_ytm = FALSE,
    weighting = "inverse_duration"
  )
  dl_estimate <- estimate_dl_from_quotes(
    bonds = bonds,
    quotes = quotes,
    lambda = 0.6,
    weighting = "inverse_duration"
  )

  for (fit_values in list(
    ns_estimate$fit$fitted_values,
    nss_estimate$fit$fitted_values,
    dl_estimate$fitted_values
  )) {
    expect_equal(sum(fit_values$objective_weight), 1)
    expect_true(all(fit_values$duration > 0))
    expect_gt(fit_values$objective_weight[1], fit_values$objective_weight[nrow(fit_values)])
  }
})

test_that("fit_nss_prices returns price-based NSS fit", {
  settlement_date <- as.Date("2026-01-01")
  maturities <- as.Date(c(
    "2026-07-01", "2027-01-01", "2028-01-01",
    "2030-01-01", "2033-01-01", "2036-01-01"
  ))
  bonds <- data.frame(
    bond_id = paste0("Z", seq_along(maturities)),
    issue_date = rep(settlement_date, length(maturities)),
    maturity = maturities,
    coupon_rate = 0,
    coupon_frequency = 1L
  )
  time <- year_fraction(settlement_date, maturities, "ACT/365.25")
  zero_rates <- nss_yield(time, 0.035, -0.015, 0.01, 0.005, 0.6, 1.8)
  clean_prices <- 100 * (1 + zero_rates)^(-time)
  quotes <- make_price_panel(settlement_date, bonds$bond_id, clean_prices)

  fit <- fit_nss_prices(
    bonds = bonds,
    quotes = quotes,
    face_value = 100,
    start = c(beta0 = 0.035, beta1 = -0.015, beta2 = 0.01, beta3 = 0.005, lambda1 = 0.6, lambda2 = 1.8)
  )

  expect_s3_class(fit, "yc_nss_price")
  expect_named(
    fit$coefficients,
    c("date", "beta0", "beta1", "beta2", "beta3", "lambda1", "lambda2")
  )
  expect_equal(fit$convergence$code, 0)
  expect_lt(max(abs(fit$fitted_values$price_residual)), 1e-6)
})

test_that("fit_nss_dirty_prices uses previous day coefficients as next start", {
  settlement_1 <- as.Date("2026-01-01")
  settlement_2 <- as.Date("2026-01-02")
  maturities <- as.Date(c(
    "2026-07-01", "2027-01-01", "2028-01-01",
    "2030-01-01", "2033-01-01", "2036-01-01"
  ))
  bonds <- data.frame(
    bond_id = paste0("Z", seq_along(maturities)),
    issue_date = rep(settlement_1, length(maturities)),
    maturity = maturities,
    coupon_rate = 0,
    coupon_frequency = 1L
  )

  par_1 <- c(beta0 = 0.035, beta1 = -0.015, beta2 = 0.01, beta3 = 0.005, lambda1 = 0.6, lambda2 = 1.8)
  time_1 <- year_fraction(settlement_1, maturities, "ACT/365.25")
  time_2 <- year_fraction(settlement_2, maturities, "ACT/365.25")
  prices_1 <- 100 * (1 + nss_yield(time_1, par_1[1], par_1[2], par_1[3], par_1[4], par_1[5], par_1[6]))^(-time_1)
  prices_2 <- 100 * (1 + nss_yield(time_2, par_1[1], par_1[2], par_1[3], par_1[4], par_1[5], par_1[6]))^(-time_2)
  dirty_prices <- make_price_panel(
    c(settlement_1, settlement_2),
    bonds$bond_id,
    rbind(prices_1, prices_2)
  )

  fit <- fit_nss_dirty_prices(
    bonds = bonds,
    dirty_prices = dirty_prices,
    face_value = 100,
    start = par_1
  )

  expect_s3_class(fit, "yc_nss_price")
  expect_equal(fit$convergence$code, c(0, 0))
  expect_equal(fit$convergence$start_source, c("user", "previous_date"))
  expect_lt(max(abs(fit$fitted_values$price_residual)), 1e-6)
})

test_that("estimate_nss_from_quotes returns estimation outputs", {
  settlement_date <- as.Date("2026-01-01")
  maturities <- as.Date(c(
    "2026-07-01", "2027-01-01", "2028-01-01",
    "2030-01-01", "2033-01-01", "2036-01-01"
  ))
  bonds <- data.frame(
    bond_id = paste0("Z", seq_along(maturities)),
    issue_date = rep(settlement_date, length(maturities)),
    maturity = maturities,
    coupon_rate = 0,
    coupon_frequency = 1L
  )
  time <- year_fraction(settlement_date, maturities, "ACT/365.25")
  zero_rates <- nss_yield(time, 0.035, -0.015, 0.01, 0.005, 0.6, 1.8)
  clean_prices <- 100 * (1 + zero_rates)^(-time)
  quotes <- make_price_panel(settlement_date, bonds$bond_id, clean_prices)

  est <- estimate_nss_from_quotes(
    bonds = bonds,
    quotes = quotes,
    face_value = 100,
    start = c(beta0 = 0.035, beta1 = -0.015, beta2 = 0.01, beta3 = 0.005, lambda1 = 0.6, lambda2 = 1.8)
  )

  expect_s3_class(est, "yc_nss_estimate")
  expect_equal(nrow(est$dirty_prices), 1)
  expect_s3_class(est$fit, "yc_nss_price")
  expect_equal(nrow(est$ytm), 6)
})

test_that("plot_nss_term_structure returns curve and observed points", {
  settlement_date <- as.Date("2026-01-01")
  maturities <- as.Date(c(
    "2026-07-01", "2027-01-01", "2028-01-01",
    "2030-01-01", "2033-01-01", "2036-01-01"
  ))
  bonds <- data.frame(
    bond_id = paste0("Z", seq_along(maturities)),
    issue_date = rep(settlement_date, length(maturities)),
    maturity = maturities,
    coupon_rate = 0,
    coupon_frequency = 1L
  )
  time <- year_fraction(settlement_date, maturities, "ACT/365.25")
  pars <- c(beta0 = 0.035, beta1 = -0.015, beta2 = 0.01, beta3 = 0.005, lambda1 = 0.6, lambda2 = 1.8)
  zero_rates <- nss_yield(time, pars[1], pars[2], pars[3], pars[4], pars[5], pars[6])
  clean_prices <- 100 * (1 + zero_rates)^(-time)
  quotes <- make_price_panel(settlement_date, bonds$bond_id, clean_prices)
  est <- estimate_nss_from_quotes(
    bonds = bonds,
    quotes = quotes,
    face_value = 100,
    start = pars
  )

  grDevices::pdf(file = tempfile(fileext = ".pdf"))
  curve <- plot_nss_term_structure(est, settlement_date, n_grid = 20)
  curve_from_s3 <- plot(est, date = settlement_date, n_grid = 20)
  grDevices::dev.off()

  expect_s3_class(curve, "ggplot")
  expect_equal(nrow(attr(curve, "curve")), 20)
  expect_s3_class(curve_from_s3, "ggplot")
  expect_equal(nrow(attr(curve_from_s3, "curve")), 20)
  expect_named(attr(curve, "curve"), c("maturity", "yield"))
  expect_equal(nrow(attr(curve, "observed")), 6)
})

test_that("plot_ns_term_structure returns curve and observed points", {
  settlement_date <- as.Date("2026-01-01")
  maturities <- as.Date(c(
    "2026-07-01", "2027-01-01", "2028-01-01",
    "2030-01-01", "2033-01-01", "2036-01-01"
  ))
  bonds <- data.frame(
    bond_id = paste0("Z", seq_along(maturities)),
    issue_date = rep(settlement_date, length(maturities)),
    maturity = maturities,
    coupon_rate = 0,
    coupon_frequency = 1L
  )
  time <- year_fraction(settlement_date, maturities, "ACT/365.25")
  pars <- c(beta0 = 0.035, beta1 = -0.015, beta2 = 0.01, lambda = 0.6)
  zero_rates <- ns_yield(time, pars[1], pars[2], pars[3], pars[4])
  clean_prices <- 100 * (1 + zero_rates)^(-time)
  quotes <- make_price_panel(settlement_date, bonds$bond_id, clean_prices)
  est <- estimate_ns_from_quotes(
    bonds = bonds,
    quotes = quotes,
    face_value = 100,
    start = pars
  )

  grDevices::pdf(file = tempfile(fileext = ".pdf"))
  curve <- plot_ns_term_structure(est, settlement_date, n_grid = 20)
  curve_from_s3 <- plot(est, date = settlement_date, n_grid = 20)
  grDevices::dev.off()

  expect_s3_class(curve, "ggplot")
  expect_equal(nrow(attr(curve, "curve")), 20)
  expect_s3_class(curve_from_s3, "ggplot")
  expect_equal(nrow(attr(curve_from_s3, "curve")), 20)
  expect_named(attr(curve, "curve"), c("maturity", "yield"))
  expect_equal(nrow(attr(curve, "observed")), 6)
})

test_that("plot_dl_term_structure returns curve and observed points", {
  settlement_date <- as.Date("2026-01-01")
  maturities <- as.Date(c(
    "2026-07-01", "2027-01-01", "2028-01-01",
    "2030-01-01", "2033-01-01", "2036-01-01"
  ))
  bonds <- data.frame(
    bond_id = paste0("Z", seq_along(maturities)),
    issue_date = rep(settlement_date, length(maturities)),
    maturity = maturities,
    coupon_rate = 0,
    coupon_frequency = 1L
  )
  time <- year_fraction(settlement_date, maturities, "ACT/365")
  observed_yields <- c(0.031, 0.033, 0.035, 0.038, 0.041, 0.043)
  clean_prices <- 100 * (1 + observed_yields)^(-time)
  quotes <- make_price_panel(settlement_date, bonds$bond_id, clean_prices)
  est <- estimate_dl_from_quotes(
    bonds = bonds,
    quotes = quotes,
    lambda = 0.1,
    face_value = 100,
    discount_day_count = "ACT/365"
  )

  grDevices::pdf(file = tempfile(fileext = ".pdf"))
  plot(est)
  curve <- plot_dl_term_structure(est, settlement_date, n_grid = 20)
  curve_from_s3 <- plot(est, type = "term_structure", date = settlement_date, n_grid = 20)
  grDevices::dev.off()

  expect_s3_class(curve, "ggplot")
  expect_equal(nrow(attr(curve, "curve")), 20)
  expect_s3_class(curve_from_s3, "ggplot")
  expect_equal(nrow(attr(curve_from_s3, "curve")), 20)
  expect_named(attr(curve, "curve"), c("maturity", "yield"))
  expect_equal(nrow(attr(curve, "observed")), 6)
})
