#' Aggregate Weather Observations to Daily Data
#'
#' Converts sub-daily weather observations to one row per day, optionally within
#' each site or location. The same `statistics` style used by the window
#' workflow is supported here: use a character vector to apply the same
#' statistics to every variable, or use a named list to choose different
#' statistics for different variables. The special `.conditions` entry can be
#' used for multivariable summaries within each daily group.
#'
#' @param weather A weather data frame.
#' @param id_col Optional site, location, or series identifier column. If
#'   supplied, aggregation is done separately within each ID.
#' @param time_col Name of the timestamp column. Use `NULL` when the input data
#'   already have a day/date column and no separate timestamp column is needed.
#' @param date_col Name of the day/date column. If this column already exists in
#'   `weather`, it is used for daily grouping. If it does not exist,
#'   `aggregate_weather_daily()` derives it from `time_col` and creates it in
#'   the output.
#' @param weather_cols Character vector of weather columns to aggregate. If
#'   named, the names are used in the output column names.
#' @param temp_col Name of the temperature column.
#' @param rh_col Name of the relative humidity column.
#' @param rain_col Name of the rainfall column.
#' @param leaf_wetness_col Name of the leaf wetness column.
#' @param statistics Daily statistics to compute. Use a character vector to
#'   apply the same statistics to all `weather_cols`, such as
#'   `c("mean", "max")`. Use a named list to choose statistics by variable or
#'   to provide custom named functions. Use `.conditions` for multivariable
#'   condition summaries.
#' @param keep_time If `TRUE`, include a daily `POSIXct` time column at midnight.
#'   This requires either `time_col` or an existing `date_col`.
#' @param name_prefix Prefix used in aggregated weather columns. The default
#'   creates names such as `daily_mean_temp` and `daily_sum_rain`.
#'
#' @return A daily data frame sorted by ID and date.
#'
#' @examples
#' hourly <- simulate_weather_series(days = 5, n_series = 2, id_col = "site_id", seed = 1)
#'
#' aggregate_weather_daily(
#'   hourly,
#'   id_col = "site_id",
#'   statistics = list(temp = c("mean", "max"), rh = "mean", rain = "sum")
#' )
#'
#' @export
aggregate_weather_daily <- function(
    weather,
    id_col = NULL,
    time_col = "time",
    date_col = "date",
    weather_cols = NULL,
    temp_col = "temp",
    rh_col = "rh",
    rain_col = "rain",
    leaf_wetness_col = "leaf_wetness",
    statistics = list(
      temp = "mean",
      rh = "mean",
      rain = "sum",
      leaf_wetness = "sum"
    ),
    keep_time = TRUE,
    name_prefix = "daily"
) {
  if (!is.null(id_col) && (!is.character(id_col) || length(id_col) != 1 || !nzchar(id_col))) {
    stop("`id_col` must be `NULL` or a single non-empty string.", call. = FALSE)
  }
  if (!is.null(time_col) && (!is.character(time_col) || length(time_col) != 1 || !nzchar(time_col))) {
    stop("`time_col` must be `NULL` or a single non-empty string.", call. = FALSE)
  }
  if (!is.character(date_col) || length(date_col) != 1 || !nzchar(date_col)) {
    stop("`date_col` must be a single non-empty string.", call. = FALSE)
  }
  if (!is.logical(keep_time) || length(keep_time) != 1 || is.na(keep_time)) {
    stop("`keep_time` must be `TRUE` or `FALSE`.", call. = FALSE)
  }
  if (!is.character(name_prefix) || length(name_prefix) != 1 || is.na(name_prefix)) {
    stop("`name_prefix` must be a single character string.", call. = FALSE)
  }

  weather_cols_was_null <- is.null(weather_cols)
  if (weather_cols_was_null && .statistics_condition_only(statistics)) {
    weather_cols <- stats::setNames(character(), character())
  } else if (is.character(weather_cols) && length(weather_cols) == 0) {
    weather_cols <- stats::setNames(character(), character())
  } else {
    weather_cols <- .resolve_weather_cols(
      weather_cols,
      temp_col = temp_col,
      rh_col = rh_col,
      rain_col = rain_col,
      leaf_wetness_col = leaf_wetness_col
    )
  }
  weather_cols <- .extend_weather_cols_from_statistics(
    weather_cols,
    statistics,
    names(weather),
    replace_defaults = weather_cols_was_null
  )

  if (!is.data.frame(weather)) {
    stop("`weather` must be a data frame.", call. = FALSE)
  }
  has_date_col <- date_col %in% names(weather)
  if (is.null(time_col) && !has_date_col) {
    stop("`date_col` must exist in `weather` when `time_col = NULL`.", call. = FALSE)
  }

  required_cols <- unname(weather_cols)
  if (!is.null(time_col)) {
    required_cols <- c(time_col, required_cols)
  }
  if (has_date_col) {
    required_cols <- c(date_col, required_cols)
  }
  if (!is.null(id_col)) {
    required_cols <- c(id_col, required_cols)
  }
  missing_cols <- setdiff(required_cols, names(weather))
  if (length(missing_cols) > 0) {
    stop(
      sprintf("Missing required columns: %s", paste(missing_cols, collapse = ", ")),
      call. = FALSE
    )
  }

  if (!is.null(time_col) && !inherits(weather[[time_col]], "POSIXct")) {
    weather[[time_col]] <- as.POSIXct(weather[[time_col]], tz = "UTC")
  }
  if (!is.null(time_col) && any(is.na(weather[[time_col]]))) {
    stop("`time_col` could not be converted to valid POSIXct values.", call. = FALSE)
  }

  statistics <- .resolve_window_statistics(statistics, names(weather_cols))
  if (has_date_col) {
    weather$.windcut_date <- as.Date(weather[[date_col]])
    if (any(is.na(weather$.windcut_date))) {
      stop("`date_col` could not be converted to valid Date values.", call. = FALSE)
    }
  } else {
    weather$.windcut_date <- as.Date(weather[[time_col]])
  }

  group_cols <- if (is.null(id_col)) ".windcut_date" else c(id_col, ".windcut_date")
  split_key <- interaction(weather[group_cols], drop = TRUE, lex.order = TRUE)
  split_weather <- split(weather, split_key)

  rows <- lapply(split_weather, function(group_data) {
    out <- data.frame(.windcut_row = 1L, stringsAsFactors = FALSE)
    if (!is.null(id_col)) {
      out[[id_col]] <- group_data[[id_col]][[1]]
    }
    out[[date_col]] <- group_data$.windcut_date[[1]]
    if (keep_time) {
      if (is.null(time_col)) {
        stop("`keep_time = TRUE` requires `time_col` when `time_col = NULL`.", call. = FALSE)
      }
      out[[time_col]] <- as.POSIXct(out[[date_col]], tz = "UTC")
    }

    for (variable in names(statistics)) {
      variable_stats <- statistics[[variable]]
      for (statistic_name in names(variable_stats)) {
        if (identical(variable, ".conditions")) {
          output_name <- .daily_output_name(
            variable = "",
            statistic = statistic_name,
            name_prefix = name_prefix
          )
          out[[output_name]] <- .compute_condition_statistic(
            group_data,
            variable_stats[[statistic_name]]
          )
        } else {
          output_name <- .daily_output_name(
            variable = variable,
            statistic = statistic_name,
            name_prefix = name_prefix
          )
          out[[output_name]] <- .compute_window_statistic(
            group_data[[weather_cols[[variable]]]],
            variable_stats[[statistic_name]]
          )
        }
      }
    }

    out
  })

  out <- do.call(rbind, rows)
  out$.windcut_row <- NULL
  rownames(out) <- NULL
  order_cols <- if (is.null(id_col)) date_col else c(id_col, date_col)
  out <- out[do.call(order, out[order_cols]), , drop = FALSE]
  rownames(out) <- NULL
  out
}

.daily_output_name <- function(variable, statistic, name_prefix) {
  pieces <- c(name_prefix, statistic, variable)
  pieces <- pieces[nzchar(pieces)]
  paste(pieces, collapse = "_")
}
