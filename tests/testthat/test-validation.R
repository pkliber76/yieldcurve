test_that("input validation accepts valid inputs", {
  bonds <- data.frame(
    bond_id = c("B1", "B2"),
    issue_date = as.Date(c("2025-01-01", "2025-01-01")),
    maturity = as.Date(c("2030-01-01", "2031-01-01")),
    coupon_rate = c(0.03, 0.04),
    coupon_frequency = c(2L, 1L)
  )
  quotes <- data.frame(
    date = as.Date("2026-01-01"),
    B1 = 99.5,
    B2 = 101.2
  )

  expect_true(validate_bonds(bonds))
  expect_true(validate_quotes(quotes))
  expect_true(validate_inputs(bonds, quotes))
  expect_equal(as_quote_data_frame(quotes)$date, as.Date(c("2026-01-01", "2026-01-01")))
})

test_that("input validation rejects unknown quoted bonds", {
  bonds <- data.frame(
    bond_id = "B1",
    issue_date = as.Date("2025-01-01"),
    maturity = as.Date("2030-01-01"),
    coupon_rate = 0.03,
    coupon_frequency = 2L
  )
  quotes <- data.frame(
    date = as.Date("2026-01-01"),
    B2 = 99.5
  )

  expect_error(validate_inputs(bonds, quotes), "not present in bonds")
})

test_that("prepare_bond_data removes expired observations", {
  bonds <- data.frame(
    bond_id = c("B1", "B2"),
    issue_date = as.Date(c("2020-01-01", "2020-01-01")),
    maturity = as.Date(c("2025-01-01", "2030-01-01")),
    coupon_rate = c(0.03, 0.04),
    coupon_frequency = c(2L, 1L)
  )
  quotes <- data.frame(
    date = as.Date("2026-01-01"),
    B1 = 99.5,
    B2 = 101.2
  )

  prepared <- prepare_bond_data(bonds, quotes)
  expect_equal(prepared$bond_id, "B2")
  expect_true(prepared$time_to_maturity > 0)
})

test_that("bond validation rejects issue dates after maturity", {
  bonds <- data.frame(
    bond_id = "B1",
    issue_date = as.Date("2030-01-01"),
    maturity = as.Date("2030-01-01"),
    coupon_rate = 0.03,
    coupon_frequency = 2L
  )

  expect_error(validate_bonds(bonds), "earlier than")
})

test_that("quote validation rejects duplicate quote dates", {
  quotes <- data.frame(
    date = as.Date(c("2026-01-01", "2026-01-01")),
    B1 = c(99, 100)
  )

  expect_error(validate_quotes(quotes), "unique")
})

test_that("quote validation accepts multiple quote dates", {
  quotes <- data.frame(
    date = as.Date(c("2026-01-01", "2026-01-02")),
    B1 = c(99, 100)
  )

  quotes_df <- as_quote_data_frame(quotes)

  expect_true(validate_quotes(quotes))
  expect_equal(quotes_df$date, as.Date(c("2026-01-01", "2026-01-02")))
  expect_equal(quotes_df$clean_price, c(99, 100))
})

test_that("prepare_clean_prices validates and returns a wide price panel", {
  prices <- data.frame(
    date = as.Date(c("2027-01-01", "2027-01-02")),
    B1 = c(99.5, 99.6),
    B2 = c(101.2, NA)
  )

  quotes <- prepare_clean_prices(prices)

  expect_true(validate_quotes(quotes))
  expect_s3_class(quotes, "data.frame")
  expect_named(quotes, c("date", "B1", "B2"))
  expect_equal(quotes$date, as.Date(c("2027-01-01", "2027-01-02")))
  expect_equal(as_quote_data_frame(quotes)$clean_price, c(99.5, 101.2, 99.6))
})

test_that("prepare_clean_prices removes observations outside bond life", {
  bonds <- data.frame(
    bond_id = c("B1", "B2"),
    issue_date = as.Date(c("2027-01-01", "2027-01-02")),
    maturity = as.Date(c("2027-01-03", "2027-01-04")),
    coupon_rate = c(0.03, 0.04),
    coupon_frequency = c(2L, 1L)
  )
  prices <- data.frame(
    date = as.Date(c("2026-12-31", "2027-01-01", "2027-01-02", "2027-01-03")),
    B1 = c(99, 100, 101, 102),
    B2 = c(98, 99, 100, 101)
  )

  quotes <- prepare_clean_prices(prices, bonds = bonds)
  quotes_df <- as_quote_data_frame(quotes)

  expect_s3_class(quotes, "data.frame")
  expect_true(is.na(quotes$B1[quotes$date == as.Date("2027-01-03")]))
  expect_true(is.na(quotes$B2[quotes$date == as.Date("2027-01-01")]))
  expect_equal(quotes_df$date, as.Date(c("2027-01-01", "2027-01-02", "2027-01-02", "2027-01-03")))
  expect_equal(quotes_df$bond_id, c("B1", "B1", "B2", "B2"))
  expect_equal(quotes_df$clean_price, c(100, 101, 100, 101))
})

test_that("prices_to_wide validates and reorders wide price panels", {
  clean_prices <- data.frame(
    date = as.Date(c("2027-01-01", "2027-01-02")),
    B1 = c(99, 99.2),
    B2 = c(NA, 101)
  )
  dirty_prices <- data.frame(
    date = as.Date("2027-01-01"),
    B1 = 99.5,
    B2 = 101.5
  )

  clean_wide <- prices_to_wide(clean_prices, bond_id = c("B2", "B1"))
  dirty_wide <- prices_to_wide(dirty_prices)

  expect_named(clean_wide, c("date", "B2", "B1"))
  expect_equal(clean_wide$date, as.Date(c("2027-01-01", "2027-01-02")))
  expect_equal(clean_wide$B1, c(99, 99.2))
  expect_true(is.na(clean_wide$B2[1]))
  expect_equal(clean_wide$B2[2], 101)
  expect_named(dirty_wide, c("date", "B1", "B2"))
  expect_equal(dirty_wide$B2, 101.5)
})
