test_that("generate_cash_flows creates coupon and principal flows", {
  bonds <- data.frame(
    bond_id = "B1",
    issue_date = as.Date("2026-01-01"),
    maturity = as.Date("2027-01-01"),
    coupon_rate = 0.04,
    coupon_frequency = 2L
  )

  cash_flows <- generate_cash_flows(bonds, face_value = 100)

  expect_equal(cash_flows$cash_flow_date, as.Date(c("2026-07-01", "2027-01-01")))
  expect_equal(cash_flows$coupon_amount, c(2, 2))
  expect_equal(cash_flows$principal_amount, c(0, 100))
  expect_equal(cash_flows$cash_flow, c(2, 102))
})

test_that("generate_cash_flows filters by settlement date", {
  bonds <- data.frame(
    bond_id = "B1",
    issue_date = as.Date("2026-01-01"),
    maturity = as.Date("2027-01-01"),
    coupon_rate = 0.04,
    coupon_frequency = 2L
  )

  cash_flows <- generate_cash_flows(
    bonds,
    face_value = 100,
    settlement_date = as.Date("2026-07-01")
  )

  expect_equal(cash_flows$cash_flow_date, as.Date("2027-01-01"))
  expect_equal(cash_flows$cash_flow, 102)
})

test_that("generate_cash_flows handles zero-coupon bonds", {
  bonds <- data.frame(
    bond_id = "Z1",
    issue_date = as.Date("2026-01-01"),
    maturity = as.Date("2027-01-01"),
    coupon_rate = 0,
    coupon_frequency = 1L
  )

  cash_flows <- generate_cash_flows(bonds, face_value = 1000)

  expect_equal(nrow(cash_flows), 1)
  expect_equal(cash_flows$cash_flow_type, "principal")
  expect_equal(cash_flows$cash_flow, 1000)
})
