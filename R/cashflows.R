#' Generate Bond Cash Flows
#'
#' Creates contractual coupon and principal cash flows from bond reference data.
#' Coupon dates are generated on a regular schedule anchored at maturity and
#' filtered to dates after each bond's `issue_date`. If `settlement_date` is
#' supplied, only cash flows after that date are returned.
#'
#' @param bonds A data frame with columns `bond_id`, `issue_date`, `maturity`,
#'   `coupon_rate`, and `coupon_frequency`.
#' @param face_value Numeric face value. Use a single value for all bonds or one
#'   value per row of `bonds`. Defaults to `100`.
#' @param settlement_date Optional `Date` scalar. If supplied, cash flows on or
#'   before this date are removed.
#'
#' @return A data frame with columns `bond_id`, `cash_flow_date`,
#'   `period_number`, `cash_flow_type`, `coupon_amount`, `principal_amount`,
#'   and `cash_flow`.
#' @export
#'
#' @examples
#' bonds <- data.frame(
#'   bond_id = c("GOV_2Y", "CORP_5Y", "MUNI_7Y", "ZERO_3Y"),
#'   issue_date = as.Date(c(
#'     "2026-01-15",
#'     "2025-06-30",
#'     "2024-03-31",
#'     "2026-07-01"
#'   )),
#'   maturity = as.Date(c(
#'     "2028-01-15",
#'     "2030-06-30",
#'     "2031-03-31",
#'     "2029-07-01"
#'   )),
#'   coupon_rate = c(0.035, 0.0525, 0.041, 0),
#'   coupon_frequency = c(2L, 4L, 1L, 1L)
#' )
#'
#' face_values <- c(100, 1000, 5000, 100)
#'
#' all_cash_flows <- generate_cash_flows(
#'   bonds = bonds,
#'   face_value = face_values
#' )
#' all_cash_flows
#'
#' future_cash_flows <- generate_cash_flows(
#'   bonds = bonds,
#'   face_value = face_values,
#'   settlement_date = as.Date("2027-01-01")
#' )
#' future_cash_flows
#'
#' aggregate(cash_flow ~ bond_id, data = future_cash_flows, FUN = sum)
#'
#' future_cash_flows[
#'   future_cash_flows$cash_flow_type %in% c("principal", "coupon_principal"),
#' ]
generate_cash_flows <- function(bonds, face_value = 100, settlement_date = NULL) {
  validate_bonds(bonds)

  if (!is.numeric(face_value) || any(!is.finite(face_value)) || any(face_value <= 0)) {
    stop("face_value must contain positive numeric values.", call. = FALSE)
  }
  if (length(face_value) == 1) {
    face_value <- rep(face_value, nrow(bonds))
  }
  if (length(face_value) != nrow(bonds)) {
    stop("face_value must have length 1 or one value per row of bonds.", call. = FALSE)
  }

  if (!is.null(settlement_date)) {
    if (!inherits(settlement_date, "Date") || length(settlement_date) != 1 || is.na(settlement_date)) {
      stop("settlement_date must be NULL or a single non-missing Date.", call. = FALSE)
    }
  }

  rows <- lapply(seq_len(nrow(bonds)), function(i) {
    bond_cash_flows(bonds[i, , drop = FALSE], face_value[i], settlement_date)
  })

  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

bond_cash_flows <- function(bond, face_value, settlement_date) {
  frequency <- as.integer(bond$coupon_frequency)
  if (12 %% frequency != 0) {
    stop(
      "coupon_frequency must divide 12 to generate calendar cash-flow dates.",
      call. = FALSE
    )
  }

  if (bond$coupon_rate == 0) {
    dates <- bond$maturity
  } else {
    dates <- coupon_schedule_dates(
      issue_date = bond$issue_date,
      maturity = bond$maturity,
      coupon_frequency = frequency
    )
  }

  if (!is.null(settlement_date)) {
    dates <- dates[dates > settlement_date]
  }

  if (length(dates) == 0) {
    return(data.frame(
      bond_id = character(),
      cash_flow_date = as.Date(character()),
      period_number = integer(),
      cash_flow_type = character(),
      coupon_amount = numeric(),
      principal_amount = numeric(),
      cash_flow = numeric()
    ))
  }

  coupon_amount <- rep(face_value * bond$coupon_rate / frequency, length(dates))
  principal_amount <- ifelse(dates == bond$maturity, face_value, 0)
  cash_flow_type <- ifelse(
    principal_amount > 0 & coupon_amount > 0,
    "coupon_principal",
    ifelse(principal_amount > 0, "principal", "coupon")
  )

  data.frame(
    bond_id = bond$bond_id,
    cash_flow_date = dates,
    period_number = seq_along(dates),
    cash_flow_type = cash_flow_type,
    coupon_amount = coupon_amount,
    principal_amount = principal_amount,
    cash_flow = coupon_amount + principal_amount
  )
}

coupon_schedule_dates <- function(issue_date, maturity, coupon_frequency) {
  step_months <- 12 / coupon_frequency
  issue_year <- as.integer(format(issue_date, "%Y"))
  issue_month <- as.integer(format(issue_date, "%m"))
  maturity_year <- as.integer(format(maturity, "%Y"))
  maturity_month <- as.integer(format(maturity, "%m"))
  month_span <- (maturity_year - issue_year) * 12L + maturity_month - issue_month
  period_count <- max(0L, ceiling(month_span / step_months) + 1L)
  offsets <- -step_months * seq.int(0L, period_count)
  dates <- add_months(rep(maturity, length(offsets)), offsets)
  dates <- dates[dates > issue_date]
  sort(unique(dates))
}

add_months <- function(date, months) {
  date <- as.Date(date)
  months <- as.integer(months)

  year <- as.integer(format(date, "%Y"))
  month <- as.integer(format(date, "%m"))
  day <- as.integer(format(date, "%d"))

  raw_month <- month + months
  out_year <- year + floor((raw_month - 1) / 12)
  out_month <- ((raw_month - 1) %% 12) + 1
  out_day <- pmin(day, days_in_month(out_year, out_month))

  as.Date(sprintf("%04d-%02d-%02d", out_year, out_month, out_day))
}

days_in_month <- function(year, month) {
  days <- c(31L, 28L, 31L, 30L, 31L, 30L, 31L, 31L, 30L, 31L, 30L, 31L)[month]
  leap <- month == 2L & ((year %% 4L == 0L & year %% 100L != 0L) | year %% 400L == 0L)
  days[leap] <- 29L
  days
}
