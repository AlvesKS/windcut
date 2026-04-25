#' Validate a Weather Time Series
#'
#' Checks that a weather table contains the expected columns, converts the time
#' column to `POSIXct`, sorts observations by time, and removes duplicated
#' timestamps.
#'
#' @param weather A data frame with weather observations.
#' @param time_col Name of the timestamp column.
#' @param required_cols Character vector of required weather columns.
#'
#' @return A cleaned data frame sorted by time.
#' @export
validate_weather_data <- function(
    weather,
    time_col = "time",
    required_cols = c("temp", "rh", "rain", "leaf_wetness")
) {
  if (!is.data.frame(weather)) {
    stop("`weather` must be a data frame.", call. = FALSE)
  }

  missing_cols <- setdiff(c(time_col, required_cols), names(weather))
  if (length(missing_cols) > 0) {
    stop(
      sprintf("Missing required columns: %s", paste(missing_cols, collapse = ", ")),
      call. = FALSE
    )
  }

  time_values <- weather[[time_col]]
  if (!inherits(time_values, "POSIXct")) {
    time_values <- as.POSIXct(time_values, tz = "UTC")
  }

  if (any(is.na(time_values))) {
    stop("`time_col` could not be converted to valid POSIXct values.", call. = FALSE)
  }

  weather[[time_col]] <- time_values
  weather <- weather[order(weather[[time_col]]), , drop = FALSE]
  weather <- weather[!duplicated(weather[[time_col]]), , drop = FALSE]
  rownames(weather) <- NULL

  weather
}

