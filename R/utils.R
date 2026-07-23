check_required_columns <- function(x, required, object_name) {
  missing_cols <- setdiff(required, names(x))
  if (length(missing_cols) > 0) {
    stop(
      object_name, " is missing required column(s): ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

check_numeric_vector <- function(x, name, positive = FALSE, nonnegative = FALSE) {
  if (!is.numeric(x)) {
    stop(name, " must be numeric.", call. = FALSE)
  }
  if (any(!is.finite(x))) {
    stop(name, " must contain only finite values.", call. = FALSE)
  }
  if (positive && any(x <= 0)) {
    stop(name, " must contain only positive values.", call. = FALSE)
  }
  if (nonnegative && any(x < 0)) {
    stop(name, " must contain only non-negative values.", call. = FALSE)
  }
  invisible(TRUE)
}

as_maturity <- function(maturity) {
  maturity <- as.numeric(maturity)
  check_numeric_vector(maturity, "maturity", nonnegative = TRUE)
  maturity
}

ns_l1 <- function(maturity, lambda) {
  x <- lambda * maturity
  out <- (1 - exp(-x)) / x
  out[x == 0] <- 1
  out
}

ns_l2 <- function(maturity, lambda) {
  ns_l1(maturity, lambda) - exp(-lambda * maturity)
}

validate_lambda <- function(lambda, name = "lambda") {
  check_numeric_vector(lambda, name, positive = TRUE)
  if (length(lambda) != 1) {
    stop(name, " must be a single positive numeric value.", call. = FALSE)
  }
  invisible(TRUE)
}

create_progress_bar <- function(progress, total, label) {
  if (!isTRUE(progress)) {
    return(NULL)
  }
  if (!is.numeric(total) || length(total) != 1 || total <= 0) {
    return(NULL)
  }

  if (!is.null(label) && nzchar(label)) {
    message(label)
  }
  utils::txtProgressBar(min = 0, max = total, style = 3)
}

update_progress_bar <- function(progress_bar, value) {
  if (!is.null(progress_bar)) {
    utils::setTxtProgressBar(progress_bar, value)
  }
  invisible(NULL)
}

close_progress_bar <- function(progress_bar) {
  if (!is.null(progress_bar)) {
    close(progress_bar)
  }
  invisible(NULL)
}
