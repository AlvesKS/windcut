#' Scan Candidate Weather Windows Relative to a Reference Time
#'
#' For each candidate window, subsets the weather series relative to a reference
#' time and computes summary features. The reference can be an assessment date,
#' planting date, flowering date, inoculation date, or any other biologically
#' meaningful timestamp.
#'
#' @param weather A weather data frame.
#' @param reference_time Reference time used to place the candidate windows.
#' @param windows A data frame created by [make_windows()].
#' @param weather_cols Character vector of weather columns to summarize. If
#'   named, the names are used in the output feature names.
#' @param time_col Name of the weather timestamp column.
#' @param temp_col Name of the temperature column.
#' @param rh_col Name of the relative humidity column. This is only used when
#'   `weather_cols = NULL`.
#' @param rain_col Name of the rainfall column.
#' @param leaf_wetness_col Name of the leaf wetness column.
#' @param unit Time unit for relative-time windows. One of `"days"` or
#'   `"hours"`.
#' @param rh_threshold Optional threshold value retained in the API. Threshold
#'   counts are computed through `statistics`, for example
#'   `list(rh = list(days_ge_90 = count_ge(90)))`.
#' @param statistics Summary statistics passed to [summarise_weather_window()].
#'
#' @return A data frame with one row per candidate window and summary metrics.
#' @export
scan_windows <- function(
    weather,
    reference_time,
    windows,
    weather_cols = NULL,
    time_col = "time",
    temp_col = "temp",
    rh_col = "rh",
    rain_col = "rain",
    leaf_wetness_col = "leaf_wetness",
    unit = c("days", "hours"),
    rh_threshold = 90,
    statistics = list(
      temp = c("mean", "min", "max"),
      rh = "mean",
      rain = "sum",
      leaf_wetness = "sum"
    )
) {
  unit <- match.arg(unit)
  weather_cols <- .resolve_weather_cols(
    weather_cols,
    temp_col = temp_col,
    rh_col = rh_col,
    rain_col = rain_col,
    leaf_wetness_col = leaf_wetness_col
  )
  weather <- validate_weather_data(
    weather,
    time_col = time_col,
    required_cols = unname(weather_cols)
  )

  if (!inherits(reference_time, "POSIXct")) {
    reference_time <- as.POSIXct(reference_time, tz = "UTC")
  }

  if (nrow(windows) == 0) {
    return(windows)
  }

  seconds_per_unit <- if (identical(unit, "days")) 86400 else 3600
  rows <- vector("list", nrow(windows))

  for (i in seq_len(nrow(windows))) {
    relative_start <- windows$relative_start[[i]]
    relative_end <- windows$relative_end[[i]]

    window_start <- reference_time + (relative_start * seconds_per_unit)
    window_end <- reference_time + (relative_end * seconds_per_unit)

    inside <- weather[[time_col]] >= window_start & weather[[time_col]] < window_end
    summary_row <- summarise_weather_window(
      weather = weather[inside, , drop = FALSE],
      weather_cols = weather_cols,
      temp_col = temp_col,
      rh_col = rh_col,
      rain_col = rain_col,
      leaf_wetness_col = leaf_wetness_col,
      rh_threshold = rh_threshold,
      statistics = statistics
    )

    rows[[i]] <- cbind(
      windows[i, , drop = FALSE],
      window_start = window_start,
      window_end = window_end,
      summary_row,
      row.names = NULL
    )
  }

  do.call(rbind, rows)
}

