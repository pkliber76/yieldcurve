test_that("calculate_yields can be reused by estimation functions", {
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

  yields <- calculate_yields(bonds, quotes)
  ns_fit <- estimate_ns_from_quotes(bonds, quotes, yields = yields)
  nss_fit <- estimate_nss_from_quotes(bonds, quotes, yields = yields)
  dl_fit <- estimate_dl_from_quotes(bonds, quotes, yields = yields)

  expect_equal(ns_fit$ytm, yields)
  expect_equal(nss_fit$ytm, yields)
  expect_equal(dl_fit$ytm, yields)
  expect_equal(dl_fit$factor_input$yield, yields$ytm)
})

test_that("estimation functions validate supplied yields", {
  settlement <- as.Date("2026-01-01")
  bonds <- data.frame(
    bond_id = paste0("Z", seq_len(4)),
    issue_date = rep(as.Date("2025-01-01"), 4),
    maturity = as.Date(c("2026-07-01", "2027-01-01", "2028-01-01", "2030-01-01")),
    coupon_rate = 0,
    coupon_frequency = 1L
  )
  time <- year_fraction(settlement, bonds$maturity)
  quotes <- data.frame(
    date = settlement,
    stats::setNames(as.list(100 * (1 + 0.03)^(-time)), bonds$bond_id)
  )

  yields <- calculate_yields(bonds, quotes)
  yields$bond_id[1] <- "UNKNOWN"

  expect_error(
    estimate_ns_from_quotes(bonds, quotes, yields = yields),
    "not present in bonds"
  )
})
