test_that("fit_ns_yields returns yc_ns objects", {
  maturity <- c(0.5, 1, 2, 5, 10, 20)
  yield <- ns_yield(maturity, 0.03, -0.02, 0.01, 0.7)
  fit <- fit_ns_yields(maturity, yield)

  expect_s3_class(fit, "yc_ns")
  expect_named(fit$coefficients, c("beta0", "beta1", "beta2", "lambda"))
  expect_length(predict(fit, maturity), length(maturity))
})

test_that("fit_nss_yields returns yc_nss objects", {
  maturity <- c(0.5, 1, 2, 5, 10, 20, 30)
  yield <- nss_yield(maturity, 0.03, -0.02, 0.01, 0.003, 0.7, 1.8)
  fit <- fit_nss_yields(maturity, yield)

  expect_s3_class(fit, "yc_nss")
  expect_named(fit$coefficients, c("beta0", "beta1", "beta2", "beta3", "lambda1", "lambda2"))
  expect_length(predict(fit, maturity), length(maturity))
})
