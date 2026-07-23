test_that("Diebold-Li factor extraction recovers factors", {
  maturity <- c(0.5, 1, 2, 5, 10)
  lambda <- 0.0609
  loadings <- dl_loadings(maturity, lambda)

  factors_1 <- c(level = 0.04, slope = -0.02, curvature = 0.01)
  factors_2 <- c(level = 0.035, slope = -0.015, curvature = 0.008)

  data <- rbind(
    data.frame(
      date = as.Date("2026-01-01"),
      maturity = maturity,
      yield = as.numeric(loadings %*% factors_1)
    ),
    data.frame(
      date = as.Date("2026-01-02"),
      maturity = maturity,
      yield = as.numeric(loadings %*% factors_2)
    )
  )

  got <- fit_dl_factors(data, lambda)

  expect_equal(got$level, unname(c(factors_1["level"], factors_2["level"])), tolerance = 1e-10)
  expect_equal(got$slope, unname(c(factors_1["slope"], factors_2["slope"])), tolerance = 1e-10)
  expect_equal(got$curvature, unname(c(factors_1["curvature"], factors_2["curvature"])), tolerance = 1e-10)
})
