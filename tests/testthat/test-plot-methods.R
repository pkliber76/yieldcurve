test_that("generic plot methods are available for price and estimate objects", {
  settlement <- as.Date("2026-01-01")
  bonds <- data.frame(
    bond_id = paste0("Z", seq_len(6)),
    issue_date = rep(as.Date("2025-01-01"), 6),
    maturity = as.Date(c(
      "2026-07-01", "2027-01-01", "2028-01-01",
      "2030-01-01", "2033-01-01", "2036-01-01"
    )),
    coupon_rate = 0,
    coupon_frequency = 1L
  )
  time <- year_fraction(settlement, bonds$maturity)
  rates <- ns_yield(time, 0.03, -0.01, 0.005, 0.6)
  quotes <- data.frame(
    date = settlement,
    stats::setNames(as.list(100 * (1 + rates)^(-time)), bonds$bond_id)
  )
  dirty_prices <- calculate_dirty_prices(bonds, quotes)

  ns_price <- fit_ns_dirty_prices(bonds, dirty_prices)
  nss_price <- fit_nss_dirty_prices(bonds, dirty_prices)
  dl_price <- fit_dl_dirty_prices(bonds, dirty_prices)

  ns_estimate <- estimate_ns_from_quotes(bonds, quotes)
  nss_estimate <- estimate_nss_from_quotes(bonds, quotes)
  dl_estimate <- estimate_dl_from_quotes(bonds, quotes)

  expect_s3_class(ns_price, "yc_ns_price")
  expect_s3_class(nss_price, "yc_nss_price")
  expect_s3_class(dl_price, "yc_dl_price")

  expect_true(is.function(getS3method("plot", "yc_ns_price")))
  expect_true(is.function(getS3method("plot", "yc_nss_price")))
  expect_true(is.function(getS3method("plot", "yc_dl_price")))
  expect_true(is.function(getS3method("plot", "yc_ns_estimate")))
  expect_true(is.function(getS3method("plot", "yc_nss_estimate")))
  expect_true(is.function(getS3method("plot", "yc_dl_estimate")))

  plot_file <- tempfile(fileext = ".pdf")
  grDevices::pdf(plot_file)
  on.exit(grDevices::dev.off(), add = TRUE)

  expect_silent(plot(ns_price))
  expect_silent(plot(nss_price))
  expect_silent(plot(dl_price))
  expect_silent(plot(ns_estimate))
  expect_silent(plot(nss_estimate))
  expect_silent(plot(dl_estimate, type = "term_structure"))
})

test_that("generic term-structure plots default to the last date", {
  dates <- as.Date(c("2026-01-01", "2026-01-02"))
  bonds <- data.frame(
    bond_id = paste0("Z", seq_len(6)),
    issue_date = rep(as.Date("2025-01-01"), 6),
    maturity = as.Date(c(
      "2026-07-01", "2027-01-01", "2028-01-01",
      "2030-01-01", "2033-01-01", "2036-01-01"
    )),
    coupon_rate = 0,
    coupon_frequency = 1L
  )

  quote_rows <- lapply(seq_along(dates), function(i) {
    time <- year_fraction(dates[i], bonds$maturity)
    rates <- ns_yield(time, 0.03 + i * 0.001, -0.01, 0.005, 0.6)
    data.frame(
      date = dates[i],
      stats::setNames(as.list(100 * (1 + rates)^(-time)), bonds$bond_id)
    )
  })
  quotes <- do.call(rbind, quote_rows)
  dirty_prices <- calculate_dirty_prices(bonds, quotes)

  ns_price <- fit_ns_dirty_prices(bonds, dirty_prices)
  nss_price <- fit_nss_dirty_prices(bonds, dirty_prices)
  dl_price <- fit_dl_dirty_prices(bonds, dirty_prices)
  yields <- calculate_yields(bonds, quotes)
  ns_estimate <- estimate_ns_from_quotes(bonds, quotes, yields = yields)
  nss_estimate <- estimate_nss_from_quotes(bonds, quotes, yields = yields)
  dl_estimate <- estimate_dl_from_quotes(bonds, quotes, yields = yields)

  plot_file <- tempfile(fileext = ".pdf")
  grDevices::pdf(plot_file)
  on.exit(grDevices::dev.off(), add = TRUE)

  ns_curve <- plot(ns_price)
  nss_curve <- plot(nss_price)
  dl_curve <- plot(dl_price)
  ns_estimate_curve <- plot(ns_estimate)
  nss_estimate_curve <- plot(nss_estimate)
  dl_estimate_curve <- plot(dl_estimate, type = "term_structure")

  expect_s3_class(ns_curve, "data.frame")
  expect_s3_class(nss_curve, "data.frame")
  expect_s3_class(dl_curve, "data.frame")
  expect_equal(attr(ns_estimate_curve, "observed")$date[1], max(dates))
  expect_equal(attr(nss_estimate_curve, "observed")$date[1], max(dates))
  expect_equal(attr(dl_estimate_curve, "observed")$date[1], max(dates))
})

test_that("generic plot methods accept object, date, type argument order", {
  dates <- as.Date(c("2026-01-01", "2026-01-02"))
  bonds <- data.frame(
    bond_id = paste0("Z", seq_len(6)),
    issue_date = rep(as.Date("2025-01-01"), 6),
    maturity = as.Date(c(
      "2026-07-01", "2027-01-01", "2028-01-01",
      "2030-01-01", "2033-01-01", "2036-01-01"
    )),
    coupon_rate = 0,
    coupon_frequency = 1L
  )

  quote_rows <- lapply(seq_along(dates), function(i) {
    time <- year_fraction(dates[i], bonds$maturity)
    rates <- ns_yield(time, 0.03 + i * 0.001, -0.01, 0.005, 0.6)
    data.frame(
      date = dates[i],
      stats::setNames(as.list(100 * (1 + rates)^(-time)), bonds$bond_id)
    )
  })
  quotes <- do.call(rbind, quote_rows)
  yields <- calculate_yields(bonds, quotes)
  ns_estimate <- estimate_ns_from_quotes(bonds, quotes, yields = yields)
  nss_estimate <- estimate_nss_from_quotes(bonds, quotes, yields = yields)
  dl_estimate <- estimate_dl_from_quotes(bonds, quotes, yields = yields)

  plot_file <- tempfile(fileext = ".pdf")
  grDevices::pdf(plot_file)
  on.exit(grDevices::dev.off(), add = TRUE)

  expect_silent(plot(ns_estimate, dates[1], "term_structure"))
  expect_silent(plot(nss_estimate, dates[1], "term_structure"))
  expect_silent(plot(dl_estimate, dates[1], "term_structure"))
})
