# yieldcurve

`yieldcurve` is an initial R package for yield curve estimation. It supports
observed-yield fitting and price-based estimation from wide clean-price panels.

Implemented models:

- Nelson-Siegel
- Nelson-Siegel-Svensson
- Diebold-Li dynamic Nelson-Siegel factors with fixed lambda

## Installation

From the package root:

```r
install.packages("devtools")
devtools::install()
```

## Bond and quote preparation

```r
library(yieldcurve)

bonds <- data.frame(
  bond_id = c("B1", "B2"),
  issue_date = as.Date(c("2026-01-01", "2026-01-01")),
  maturity = as.Date(c("2030-01-01", "2035-01-01")),
  coupon_rate = c(0.03, 0.045),
  coupon_frequency = c(2L, 2L)
)

quotes <- data.frame(
  date = as.Date("2026-01-01"),
  B1 = 99.5,
  B2 = 103.2
)

validate_inputs(bonds, quotes)
prepared <- prepare_bond_data(bonds, quotes)
prepared
```

Clean prices are stored as a wide data frame, where `date` is the quote date
and each other column is named with a bond identifier:

```r
prices_wide <- data.frame(
  date = as.Date(c("2027-01-01", "2027-01-02", "2027-01-03")),
  B1 = c(99.5, 99.6, NA),
  B2 = c(101.2, NA, 101.5)
)

quotes <- prepare_clean_prices(
  prices = prices_wide,
  bonds = bonds
)

quotes
as_quote_data_frame(quotes)
```

`NA` means that a bond was not quoted on that date. If `bonds` is supplied,
prices before `issue_date` and on or after `maturity` are set to `NA`.

Prepared clean-price panels can be used by the price-based NS, NSS, and
Diebold-Li estimates below.

## Cash-flow generation

```r
cash_flows <- generate_cash_flows(bonds, face_value = 100)
cash_flows

future_cash_flows <- generate_cash_flows(
  bonds,
  face_value = 100,
  settlement_date = as.Date("2027-01-01")
)
future_cash_flows
```

A more detailed example with mixed coupon frequencies, different face values,
and a zero-coupon bond:

```r
bonds_complex <- data.frame(
  bond_id = c("GOV_2Y", "CORP_5Y", "MUNI_7Y", "ZERO_3Y", "GOV_10Y", "CORP_15Y"),
  issue_date = as.Date(c(
    "2026-01-15",
    "2025-06-30",
    "2024-03-31",
    "2026-07-01",
    "2026-01-01",
    "2025-01-01"
  )),
  maturity = as.Date(c(
    "2028-01-15",
    "2030-06-30",
    "2031-03-31",
    "2029-07-01",
    "2036-01-01",
    "2040-01-01"
  )),
  coupon_rate = c(
    0.035,
    0.0525,
    0.041,
    0,
    0.038,
    0.061
  ),
  coupon_frequency = c(
    2L,
    4L,
    1L,
    1L,
    2L,
    1L
  )
)

face_values <- c(100, 1000, 5000, 100, 100, 1000)

all_cash_flows <- generate_cash_flows(
  bonds = bonds_complex,
  face_value = face_values
)
all_cash_flows

future_cash_flows <- generate_cash_flows(
  bonds = bonds_complex,
  face_value = face_values,
  settlement_date = as.Date("2027-01-01")
)
future_cash_flows

aggregate(
  cash_flow ~ bond_id,
  data = future_cash_flows,
  FUN = sum
)

future_cash_flows[
  future_cash_flows$cash_flow_type %in% c("principal", "coupon_principal"),
]
```

## Dirty prices

Dirty prices are calculated from market clean prices by adding accrued
interest:

```r
dirty_price = clean_price + accrued_interest
```

Accrued interest is calculated from each bond's coupon schedule. By default the
function uses an ACT/ACT coupon-period fraction.

```r
quotes_complex <- data.frame(
  date = as.Date(c("2027-01-01", "2027-01-02")),
  GOV_2Y = c(100.10, 100.05),
  CORP_5Y = c(985.25, 986.10),
  MUNI_7Y = c(5020.75, 5021.40),
  ZERO_3Y = c(89.50, 89.55),
  GOV_10Y = c(101.40, 101.25),
  CORP_15Y = c(982.30, 983.15)
)

dirty_prices <- calculate_dirty_prices(
  bonds = bonds_complex,
  quotes = quotes_complex,
  face_value = face_values
)

dirty_prices
```

Accrued interest can also be inspected separately:

```r
accrued_interest <- calculate_accrued_interest(
  bonds = bonds_complex,
  prices = quotes_complex,
  face_value = face_values
)

accrued_interest
```

Clean prices can be recovered from dirty prices by subtracting accrued
interest:

```r
clean_prices_from_dirty <- calculate_clean_prices(
  bonds = bonds_complex,
  dirty_prices = dirty_prices,
  face_value = face_values
)

clean_prices_from_dirty
```

Clean-price or dirty-price panels can be validated and reordered with
`prices_to_wide()`:

```r
clean_prices_wide <- prices_to_wide(clean_prices_from_dirty)
dirty_prices_wide <- prices_to_wide(dirty_prices)

clean_prices_wide
dirty_prices_wide
```

## Yield to maturity

Yield to maturity is calculated from dirty prices and remaining contractual cash
flows. It is useful as an intermediate diagnostic, but NSS can also be fitted
directly from prices.

```r
ytm_data <- calculate_ytm(
  bonds = bonds_complex,
  quotes = quotes_complex,
  face_value = face_values
)

ytm_data
```

## Price-based NSS estimation

Use `estimate_nss_from_quotes()` for the full clean-price workflow:

Set `weighting = "inverse_duration"` to minimize a duration-weighted objective,
where each bond weight is proportional to the inverse of its duration within
the quote date.

```r
nss_estimate <- estimate_nss_from_quotes(
  bonds = bonds_complex,
  quotes = quotes_complex,
  face_value = face_values,
  weighting = "inverse_duration",
  progress = TRUE
)

nss_estimate$dirty_prices
nss_estimate$fit$coefficients
nss_estimate$fit$fitted_values[, c("bond_id", "duration", "objective_weight")]
nss_estimate$ytm

plot_nss_term_structure(
  nss_estimate,
  date = as.Date("2027-01-01")
)

plot(
  nss_estimate,
  date = as.Date("2027-01-01")
)
```

`fit_nss_prices()` estimates NSS parameters directly from clean prices. The
function first computes dirty prices, then minimizes dirty-price residuals using
cash-flow present values under the NSS zero curve. Each quote date needs at
least six bond price observations.

```r
nss_price_fit <- fit_nss_prices(
  bonds = bonds_complex,
  quotes = quotes_complex,
  face_value = face_values
)

nss_price_fit$coefficients
nss_price_fit$fitted_values
summary(nss_price_fit)
```

If dirty prices are already available, fit NSS directly to them:

```r
nss_dirty_fit <- fit_nss_dirty_prices(
  bonds = bonds_complex,
  dirty_prices = dirty_prices,
  face_value = face_values
)

nss_dirty_fit$coefficients
```

Dates are fitted sequentially: the first date uses the supplied `start` values
or package defaults, and every later date starts from the fitted NSS
coefficients of the previous date.

## Price-based Nelson-Siegel estimation

The Nelson-Siegel price estimation has the same structure as the NSS estimation,
but estimates `beta0`, `beta1`, `beta2`, and `lambda`:

```r
ns_estimate <- estimate_ns_from_quotes(
  bonds = bonds_complex,
  quotes = quotes_complex,
  face_value = face_values,
  weighting = "inverse_duration",
  progress = TRUE,
  start = c(
    beta0 = 0.04,
    beta1 = -0.02,
    beta2 = 0.01,
    lambda = 0.5
  )
)

ns_estimate$dirty_prices
ns_estimate$fit$coefficients
ns_estimate$fit$fitted_values
ns_estimate$ytm

plot_ns_term_structure(
  ns_estimate,
  date = as.Date("2027-01-01")
)

plot(
  ns_estimate,
  date = as.Date("2027-01-01")
)
```

For lower-level use, call `fit_ns_prices()` on clean prices or
`fit_ns_dirty_prices()` on precomputed dirty prices.

## Diebold-Li price-based estimation

The Diebold-Li estimation starts from the same bond and quote objects. It
calculates dirty prices, then estimates level, slope, and curvature factors by
date by minimizing dirty-price errors with a fixed `lambda`. Yield-to-maturity
observations are still returned as diagnostics and used as observed points in
term-structure plots. The same `weighting = "inverse_duration"` objective is
available here as for NS and NSS.

If `lambda` is not provided, the default value is `0.0609`:

```r
dl_estimate <- estimate_dl_from_quotes(
  bonds = bonds_complex,
  quotes = quotes_complex,
  face_value = face_values,
  weighting = "inverse_duration",
  progress = TRUE
)

dl_estimate$dirty_prices
dl_estimate$ytm
dl_estimate$factor_input
dl_estimate$factors
dl_estimate$fitted_values
dl_estimate$convergence
dl_estimate$lambda

plot(dl_estimate)
plot_dl_factors(dl_estimate$factors)
plot(
  dl_estimate,
  type = "term_structure",
  date = as.Date("2027-01-01")
)
plot_dl_term_structure(
  dl_estimate,
  date = as.Date("2027-01-01")
)
```

Use a custom `lambda` by passing it explicitly:

```r
dl_estimate_custom <- estimate_dl_from_quotes(
  bonds = bonds_complex,
  quotes = quotes_complex,
  lambda = 0.1,
  face_value = face_values,
  progress = TRUE
)

dl_estimate_custom$lambda
dl_estimate_custom$factors
```

## Nelson-Siegel

```r
maturity <- c(0.5, 1, 2, 5, 10, 20, 30)
observed_yield <- c(0.031, 0.033, 0.035, 0.038, 0.041, 0.043, 0.044)

fit <- fit_ns_yields(maturity, observed_yield)
fit
summary(fit)
predict(fit, c(1, 3, 7, 15))
plot(fit)
```

## Nelson-Siegel-Svensson

```r
nss_fit <- fit_nss_yields(maturity, observed_yield)
nss_fit
predict(nss_fit, c(1, 3, 7, 15))
plot(nss_fit)
```

## Diebold-Li Factors

```r
yield_data <- data.frame(
  date = rep(as.Date(c("2026-01-01", "2026-01-02")), each = length(maturity)),
  maturity = rep(maturity, times = 2),
  yield = c(
    observed_yield,
    observed_yield + c(0.001, 0.001, 0.0008, 0.0005, 0.0002, 0, -0.0001)
  )
)

factors <- fit_dl_factors(yield_data, lambda = 0.0609)
factors
plot_dl_factors(factors)
```

## Development

Run tests from the package root:

```r
testthat::test_dir("tests/testthat")
```
