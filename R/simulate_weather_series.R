#' Simulate a Weather Time Series
#'
#' Creates a synthetic hourly weather series with diurnal temperature and
#' humidity cycles, intermittent rain, and leaf wetness periods. This is useful
#' for examples, tutorials, tests, and method development before field data are
#' available.
#'
#' @param start Start timestamp for the simulation.
#' @param days Number of days to simulate.
#' @param n_series Number of independent weather series to generate.
#' @param seed Optional random seed for reproducibility.
#' @param id_col Optional name of the series identifier column. If `NULL` and
#'   `n_series > 1`, the output uses `"series_id"`.
#' @param id_prefix Prefix used when constructing simulated series identifiers.
#' @param temp_mean Baseline temperature.
#' @param temp_amp Amplitude of the diurnal temperature cycle.
#' @param rh_mean Baseline relative humidity.
#' @param rh_amp Amplitude of the diurnal relative humidity cycle.
#' @param rain_prob Probability of rain in each hour.
#' @param tz Time zone assigned to the simulated timestamps.
#'
#' @return A data frame with hourly weather observations.
#' @export
simulate_weather_series <- function(
    start = as.POSIXct("2024-01-01 00:00:00", tz = "UTC"),
    days = 60,
    n_series = 1,
    seed = NULL,
    id_col = NULL,
    id_prefix = "S",
    temp_mean = 22,
    temp_amp = 6,
    rh_mean = 80,
    rh_amp = 12,
    rain_prob = 0.08,
    tz = "UTC"
) {
  if (!is.numeric(days) || length(days) != 1 || days < 1) {
    stop("`days` must be a single number greater than or equal to 1.", call. = FALSE)
  }

  if (!is.numeric(n_series) || length(n_series) != 1 || n_series < 1) {
    stop("`n_series` must be a single number greater than or equal to 1.", call. = FALSE)
  }

  days <- as.integer(days)
  n_series <- as.integer(n_series)

  if (!is.null(id_col) && (!is.character(id_col) || length(id_col) != 1 || !nzchar(id_col))) {
    stop("`id_col` must be `NULL` or a single non-empty string.", call. = FALSE)
  }

  if (!is.character(id_prefix) || length(id_prefix) != 1 || !nzchar(id_prefix)) {
    stop("`id_prefix` must be a single non-empty string.", call. = FALSE)
  }

  if (!is.null(seed)) {
    set.seed(seed)
  }

  start <- as.POSIXct(start, tz = tz)
  output_id_col <- id_col
  if (is.null(output_id_col) && n_series > 1) {
    output_id_col <- "series_id"
  }

  pad_width <- max(2, nchar(as.character(n_series)))
  series_rows <- lapply(seq_len(n_series), function(i) {
    hours <- seq.POSIXt(from = start, by = "hour", length.out = days * 24)
    hour_of_day <- as.integer(format(hours, "%H"))
    day_index <- seq_along(hours) / 24

    temp <- temp_mean +
      temp_amp * sin((2 * pi * hour_of_day) / 24) +
      1.5 * sin((2 * pi * day_index) / 14) +
      stats::rnorm(length(hours), sd = 1.1)

    rh <- rh_mean +
      rh_amp * cos((2 * pi * hour_of_day) / 24) -
      0.6 * (temp - temp_mean) +
      stats::rnorm(length(hours), sd = 4)
    rh <- pmax(pmin(rh, 100), 35)

    rain_flag <- stats::runif(length(hours)) < rain_prob
    rain <- ifelse(rain_flag, stats::rexp(length(hours), rate = 0.7), 0)
    rain <- round(rain, 2)

    leaf_wetness <- as.numeric(rh >= 90 | rain > 0.1)

    out <- data.frame(
      time = hours,
      temp = round(temp, 2),
      rh = round(rh, 2),
      rain = rain,
      leaf_wetness = leaf_wetness
    )

    if (!is.null(output_id_col)) {
      out[[output_id_col]] <- sprintf(
        paste0("%s%0", pad_width, "d"),
        id_prefix,
        i
      )
      out <- out[, c(output_id_col, setdiff(names(out), output_id_col)), drop = FALSE]
    }

    out
  })

  out <- do.call(rbind, series_rows)
  rownames(out) <- NULL
  out
}

