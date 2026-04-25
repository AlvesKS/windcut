#' Example Weather Data
#'
#' Generates a small synthetic hourly weather time series that can be used in
#' examples, tests, and package demonstrations.
#'
#' @param start Start time for the series.
#' @param days Number of days to generate.
#'
#' @return A data frame with hourly weather observations.
#' @export
example_weather_data <- function(
    start = as.POSIXct("2024-01-01 00:00:00", tz = "UTC"),
    days = 30
) {
  simulate_weather_series(
    start = start,
    days = days,
    seed = 1
  )
}

