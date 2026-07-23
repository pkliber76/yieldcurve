#' Prepare Bond and Quote Data
#'
#' Joins quote observations to bond reference data, computes time to maturity in
#' years, and removes expired bonds. This function intentionally does not
#' compute yields or price-based quantities yet.
#'
#' @param bonds A data frame with columns `bond_id`, `issue_date`, `maturity`,
#'   `coupon_rate`, and `coupon_frequency`.
#' @param quotes A wide clean-price data frame accepted by [validate_quotes()].
#'
#' @return A data frame containing quote and bond columns plus
#'   `time_to_maturity`, with expired observations removed.
#' @export
#'
#' @examples
#' bonds <- data.frame(
#'   bond_id = "B1",
#'   issue_date = as.Date("2020-01-01"),
#'   maturity = as.Date("2030-01-01"),
#'   coupon_rate = 0.05,
#'   coupon_frequency = 2L
#' )
#' quotes <- data.frame(
#'   date = as.Date("2026-01-01"),
#'   B1 = 101
#' )
#' prepare_bond_data(bonds, quotes)
prepare_bond_data <- function(bonds, quotes) {
  validate_inputs(bonds, quotes)
  quotes_df <- as_quote_data_frame(quotes)

  joined <- merge(
    quotes_df,
    bonds,
    by = "bond_id",
    all.x = TRUE,
    all.y = FALSE,
    sort = FALSE
  )

  joined$time_to_maturity <- as.numeric(joined$maturity - joined$date) / 365.25
  joined <- joined[joined$time_to_maturity > 0, , drop = FALSE]

  rownames(joined) <- NULL
  joined
}
