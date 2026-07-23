test_that("NS formula is evaluated correctly", {
  maturity <- c(0, 1, 5)
  got <- ns_yield(maturity, beta0 = 0.03, beta1 = -0.02, beta2 = 0.01, lambda = 0.5)
  l1 <- c(1, (1 - exp(-0.5)) / 0.5, (1 - exp(-2.5)) / 2.5)
  l2 <- l1 - exp(-0.5 * maturity)
  expected <- 0.03 - 0.02 * l1 + 0.01 * l2
  expect_equal(got, expected)
})

test_that("NSS formula is evaluated correctly", {
  maturity <- c(0, 2, 10)
  got <- nss_yield(
    maturity,
    beta0 = 0.03,
    beta1 = -0.02,
    beta2 = 0.01,
    beta3 = 0.005,
    lambda1 = 0.5,
    lambda2 = 1.2
  )
  l1 <- c(1, (1 - exp(-1)) / 1, (1 - exp(-5)) / 5)
  l2_1 <- l1 - exp(-0.5 * maturity)
  l1_2 <- c(1, (1 - exp(-2.4)) / 2.4, (1 - exp(-12)) / 12)
  l2_2 <- l1_2 - exp(-1.2 * maturity)
  expected <- 0.03 - 0.02 * l1 + 0.01 * l2_1 + 0.005 * l2_2
  expect_equal(got, expected)
})
