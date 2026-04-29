#' Derive meteorological variables
#'
#' These functions add biologically useful variables to a weather data frame.
#' Column arguments are supplied unquoted. The original data frame is returned
#' with one additional column.
#'
#' @param data A data frame.
#' @param temp,rh,tmax,tmin Columns in `data`.
#' @param name Name of the new column.
#' @param threshold,rh_threshold Numeric threshold used to create binary wetness
#'   or favorability variables.
#' @param temp_range Optional numeric vector of length two. When supplied,
#'   estimated leaf wetness is one only when RH is high and temperature is inside
#'   this range.
#' @param condition A logical expression evaluated in `data`.
#'
#' @details
#' `derive_dew_point()` uses the Magnus approximation with constants
#' `a = 17.625` and `b = 243.04`. `derive_vpd()` computes saturation vapor
#' pressure from temperature and actual vapor pressure from relative humidity.
#' Relative humidity is interpreted as percent from 0 to 100, and VPD is returned
#' in kPa. Dew point is returned as `NA_real_` when relative humidity is zero
#' because the logarithmic Magnus approximation has no finite value at zero.
#'
#' @return A data frame with the new derived column.
#'
#' @examples
#' weather <- data.frame(temp = c(20, 24), rh = c(85, 95), rain = c(0, 1))
#'
#' weather |>
#'   derive_dew_point(temp, rh) |>
#'   derive_vpd(temp, rh) |>
#'   derive_leaf_wetness_from_rh(rh, threshold = 90)
#'
#' @name weather_derivative_functions
NULL

#' @rdname weather_derivative_functions
#' @export
derive_dew_point <- function(data, temp, rh, name = "dew_point") {
  temp_name <- .column_name(substitute(temp))
  rh_name <- .column_name(substitute(rh))
  .validate_new_column_name(name)
  .require_columns(data, c(temp_name, rh_name))
  .validate_numeric_column(data[[temp_name]], temp_name)
  .validate_relative_humidity(data[[rh_name]])

  temperature <- data[[temp_name]]
  relative_humidity <- data[[rh_name]]
  a <- 17.625
  b <- 243.04
  gamma <- rep(NA_real_, length(relative_humidity))
  valid_rh <- !is.na(relative_humidity) & relative_humidity > 0
  gamma[valid_rh] <- log(relative_humidity[valid_rh] / 100) +
    (a * temperature[valid_rh]) / (b + temperature[valid_rh])

  out <- data
  out[[name]] <- (b * gamma) / (a - gamma)
  out
}

#' @rdname weather_derivative_functions
#' @export
derive_vpd <- function(data, temp, rh, name = "vpd") {
  temp_name <- .column_name(substitute(temp))
  rh_name <- .column_name(substitute(rh))
  .validate_new_column_name(name)
  .require_columns(data, c(temp_name, rh_name))
  .validate_numeric_column(data[[temp_name]], temp_name)
  .validate_relative_humidity(data[[rh_name]])

  temperature <- data[[temp_name]]
  relative_humidity <- data[[rh_name]]
  saturation_vapor_pressure <- 0.6108 * exp((17.27 * temperature) / (temperature + 237.3))
  actual_vapor_pressure <- saturation_vapor_pressure * (relative_humidity / 100)

  out <- data
  out[[name]] <- saturation_vapor_pressure - actual_vapor_pressure
  out
}

#' @rdname weather_derivative_functions
#' @export
derive_temperature_range <- function(data, tmax, tmin, name = "temp_range") {
  tmax_name <- .column_name(substitute(tmax))
  tmin_name <- .column_name(substitute(tmin))
  .validate_new_column_name(name)
  .require_columns(data, c(tmax_name, tmin_name))
  .validate_numeric_column(data[[tmax_name]], tmax_name)
  .validate_numeric_column(data[[tmin_name]], tmin_name)

  out <- data
  out[[name]] <- out[[tmax_name]] - out[[tmin_name]]
  out
}

#' @rdname weather_derivative_functions
#' @export
derive_leaf_wetness_from_rh <- function(data, rh, threshold = 90, name = "leaf_wetness_est") {
  rh_name <- .column_name(substitute(rh))
  .validate_numeric_scalar(threshold, "threshold")
  .validate_new_column_name(name)
  .require_columns(data, rh_name)
  .validate_relative_humidity(data[[rh_name]])

  out <- data
  out[[name]] <- as.integer(out[[rh_name]] >= threshold)
  out[[name]][is.na(out[[rh_name]])] <- NA_integer_
  out
}

#' @rdname weather_derivative_functions
#' @export
derive_leaf_wetness_from_rh_temp <- function(
    data,
    rh,
    temp,
    rh_threshold = 90,
    temp_range = NULL,
    name = "leaf_wetness_est"
) {
  rh_name <- .column_name(substitute(rh))
  temp_name <- .column_name(substitute(temp))
  .validate_numeric_scalar(rh_threshold, "rh_threshold")
  .validate_new_column_name(name)
  .require_columns(data, c(rh_name, temp_name))
  .validate_numeric_column(data[[temp_name]], temp_name)
  .validate_relative_humidity(data[[rh_name]])
  if (!is.null(temp_range)) {
    if (!is.numeric(temp_range) || length(temp_range) != 2 || anyNA(temp_range) || any(!is.finite(temp_range))) {
      stop("`temp_range` must be `NULL` or a numeric vector of length two.", call. = FALSE)
    }
    if (temp_range[[1]] > temp_range[[2]]) {
      stop("`temp_range[1]` must be less than or equal to `temp_range[2]`.", call. = FALSE)
    }
  }

  condition <- data[[rh_name]] >= rh_threshold
  if (!is.null(temp_range)) {
    condition <- condition & data[[temp_name]] >= temp_range[[1]] & data[[temp_name]] <= temp_range[[2]]
  }

  out <- data
  out[[name]] <- as.integer(condition)
  out[[name]][is.na(condition)] <- NA_integer_
  out
}

#' @rdname weather_derivative_functions
#' @export
derive_favorable_condition <- function(data, condition, name = "favorable") {
  condition_expr <- substitute(condition)
  .validate_new_column_name(name)
  value <- .eval_condition(condition_expr, data)

  out <- data
  out[[name]] <- as.integer(value)
  out[[name]][is.na(value)] <- NA_integer_
  out
}

.column_name <- function(expr) {
  if (!is.symbol(expr)) {
    stop("Column arguments must be supplied as unquoted column names.", call. = FALSE)
  }
  as.character(expr)
}

.validate_new_column_name <- function(name) {
  if (!is.character(name) || length(name) != 1 || is.na(name) || !nzchar(name)) {
    stop("`name` must be a single non-empty string.", call. = FALSE)
  }
  invisible(name)
}

.validate_numeric_column <- function(x, name) {
  if (!is.numeric(x)) {
    stop(sprintf("Column `%s` must be numeric.", name), call. = FALSE)
  }
  invisible(TRUE)
}

.require_columns <- function(data, columns) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }
  missing_columns <- setdiff(columns, names(data))
  if (length(missing_columns) > 0) {
    stop(sprintf("Missing required columns: %s.", paste(missing_columns, collapse = ", ")), call. = FALSE)
  }
  invisible(TRUE)
}

.validate_relative_humidity <- function(rh) {
  if (!is.numeric(rh)) {
    stop("Relative humidity must be numeric.", call. = FALSE)
  }
  observed <- rh[!is.na(rh)]
  if (length(observed) > 0 && any(observed < 0 | observed > 100)) {
    stop("Relative humidity must be expressed as percent values from 0 to 100.", call. = FALSE)
  }
  invisible(TRUE)
}
