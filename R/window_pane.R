#' Build a Wide Feature Table from Weather Windows
#'
#' Applies [scan_windows()] to many site-specific reference dates and returns a
#' wide table with one row per reference event and one column per metric-window
#' combination. The reference date can be an assessment date, planting date, or
#' any other timestamp stored in the input table.
#'
#' @param weather A weather data frame.
#' @param assessments A data frame containing reference times and optional IDs
#'   or responses.
#' @param windows A data frame created by [make_windows()].
#' @param reference_col Name of the reference timestamp column. If `NULL`, the
#'   function uses metadata stored by [make_windows()] or falls back to
#'   `"assessment_time"`.
#' @param id_col Optional site, location, or assessment identifier column. When
#'   this column is present in both `weather` and `assessments`, each row is
#'   matched to the weather series from the same ID.
#' @param response_col Optional response column, such as disease intensity.
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
#' @return A wide data frame with one row per reference event.
#' @export
window_pane <- function(
    weather,
    assessments,
    windows,
    reference_col = NULL,
    id_col = NULL,
    response_col = NULL,
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

  if (!is.data.frame(assessments)) {
    stop("`assessments` must be a data frame.", call. = FALSE)
  }

  if (is.null(reference_col)) {
    reference_col <- attr(windows, "reference_col", exact = TRUE)
  }
  if (is.null(reference_col)) {
    reference_col <- "assessment_time"
  }

  if (!reference_col %in% names(assessments)) {
    stop(sprintf("`reference_col` '%s' was not found.", reference_col), call. = FALSE)
  }

  if (!is.null(id_col)) {
    if (!id_col %in% names(assessments)) {
      stop(sprintf("`id_col` '%s' was not found in `assessments`.", id_col), call. = FALSE)
    }
  }

  if (!is.null(response_col) && !response_col %in% names(assessments)) {
    stop(sprintf("`response_col` '%s' was not found in `assessments`.", response_col), call. = FALSE)
  }

  reference_times <- assessments[[reference_col]]
  if (!inherits(reference_times, "POSIXct")) {
    reference_times <- as.POSIXct(reference_times, tz = "UTC")
  }

  rows <- vector("list", nrow(assessments))

  for (i in seq_len(nrow(assessments))) {
    weather_i <- weather
    if (!is.null(id_col) && id_col %in% names(weather)) {
      weather_i <- weather[weather[[id_col]] == assessments[[id_col]][[i]], , drop = FALSE]
    }

    scanned <- scan_windows(
      weather = weather_i,
      reference_time = reference_times[[i]],
      windows = windows,
      weather_cols = weather_cols,
      time_col = time_col,
      temp_col = temp_col,
      rh_col = rh_col,
      rain_col = rain_col,
      leaf_wetness_col = leaf_wetness_col,
      unit = unit,
      rh_threshold = rh_threshold,
      statistics = statistics
    )

    metric_cols <- setdiff(
      names(scanned),
      c(names(windows), "window_start", "window_end")
    )
    feature_names <- as.vector(outer(metric_cols, scanned$label, paste, sep = "_"))
    feature_values <- as.vector(t(scanned[, metric_cols, drop = FALSE]))
    row <- stats::setNames(as.list(feature_values), feature_names)

    row[[reference_col]] <- reference_times[[i]]

    if (!is.null(id_col)) {
      row[[id_col]] <- assessments[[id_col]][[i]]
    }

    if (!is.null(response_col)) {
      row[[response_col]] <- assessments[[response_col]][[i]]
    }

    rows[[i]] <- as.data.frame(row, check.names = FALSE, stringsAsFactors = FALSE)
  }

  out <- do.call(rbind, rows)

  leading_cols <- c(id_col, reference_col, response_col)
  leading_cols <- leading_cols[!is.null(leading_cols)]
  trailing_cols <- setdiff(names(out), leading_cols)
  out[, c(leading_cols, trailing_cols), drop = FALSE]
}

