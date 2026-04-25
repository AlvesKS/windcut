#' Summarise Weather Inside a Single Window
#'
#' Computes a small set of epidemiology-friendly summaries from a subset of a
#' weather time series.
#'
#' @param weather A validated weather data frame.
#' @param weather_cols Character vector of weather columns to summarize. If
#'   named, the names are used in the output feature names.
#' @param temp_col Name of the temperature column.
#' @param rh_col Name of the relative humidity column. This is only used when
#'   `weather_cols = NULL`.
#' @param rain_col Name of the rainfall column.
#' @param leaf_wetness_col Name of the leaf wetness column.
#' @param rh_threshold Optional threshold value retained in the API. Threshold
#'   counts are computed through `statistics`, for example
#'   `list(rh = list(days_ge_90 = count_ge(90)))`.
#' @param statistics Summary statistics to compute. Use a character vector to
#'   apply the same statistics to all `weather_cols`, such as
#'   `c("mean", "sd", "IQR")`. Use a named list to choose statistics by
#'   variable or to provide custom named functions.
#'
#' @return A one-row data frame with summary metrics.
#' @export
summarise_weather_window <- function(
    weather,
    weather_cols = NULL,
    temp_col = "temp",
    rh_col = "rh",
    rain_col = "rain",
    leaf_wetness_col = "leaf_wetness",
    rh_threshold = 90,
    statistics = list(
      temp = c("mean", "min", "max"),
      rh = "mean",
      rain = "sum",
      leaf_wetness = "sum"
  )
) {
  weather_cols <- .resolve_weather_cols(
    weather_cols,
    temp_col = temp_col,
    rh_col = rh_col,
    rain_col = rain_col,
    leaf_wetness_col = leaf_wetness_col
  )
  statistics <- .resolve_window_statistics(statistics, names(weather_cols))
  metric_names <- .window_metric_names(statistics)

  if (nrow(weather) == 0) {
    out <- as.data.frame(
      stats::setNames(as.list(rep(NA_real_, length(metric_names))), metric_names),
      stringsAsFactors = FALSE
    )
    out <- cbind(n_obs = 0, out)
    return(out)
  }

  out <- data.frame(
    n_obs = nrow(weather),
    stringsAsFactors = FALSE
  )

  for (variable in names(statistics)) {
    for (statistic_name in names(statistics[[variable]])) {
      out[[paste(variable, statistic_name, sep = "_")]] <- .compute_window_statistic(
        weather[[weather_cols[[variable]]]],
        statistics[[variable]][[statistic_name]]
      )
    }
  }

  out
}

#' Count Values Greater Than or Equal to a Threshold
#'
#' Creates a statistic function for use in `statistics`. This is useful for
#' counts such as days with relative humidity greater than or equal to 90%.
#'
#' @param threshold Numeric threshold.
#'
#' @return A function that takes a numeric vector and returns one count.
#'
#' @examples
#' count_ge(90)(c(84, 91, 96, NA))
#'
#' @export
count_ge <- function(threshold) {
  if (!is.numeric(threshold) || length(threshold) != 1 || !is.finite(threshold)) {
    stop("`threshold` must be a single finite number.", call. = FALSE)
  }

  force(threshold)
  function(x, na.rm = TRUE) {
    if (isTRUE(na.rm)) {
      x <- x[!is.na(x)]
    }
    sum(x >= threshold, na.rm = na.rm)
  }
}

.resolve_weather_cols <- function(
    weather_cols,
    temp_col = "temp",
    rh_col = "rh",
    rain_col = "rain",
    leaf_wetness_col = "leaf_wetness"
) {
  if (is.null(weather_cols)) {
    weather_cols <- c(
      temp = temp_col,
      rh = rh_col,
      rain = rain_col,
      leaf_wetness = leaf_wetness_col
    )
  }

  if (!is.character(weather_cols) || length(weather_cols) == 0 || any(!nzchar(weather_cols))) {
    stop("`weather_cols` must be a non-empty character vector.", call. = FALSE)
  }

  if (is.null(names(weather_cols))) {
    names(weather_cols) <- weather_cols
  }
  missing_names <- !nzchar(names(weather_cols))
  names(weather_cols)[missing_names] <- weather_cols[missing_names]
  weather_cols
}

.resolve_window_statistics <- function(statistics, variables) {
  if (is.character(statistics) || is.function(statistics)) {
    statistic_spec <- .resolve_statistic_spec(statistics)
    return(stats::setNames(
      rep(list(statistic_spec), length(variables)),
      variables
    ))
  }

  if (!is.list(statistics) || is.null(names(statistics)) || any(!nzchar(names(statistics)))) {
    stop("`statistics` must be a character vector, a function, or a named list.", call. = FALSE)
  }

  if (!any(names(statistics) %in% variables)) {
    statistic_spec <- .resolve_statistic_spec(statistics)
    return(stats::setNames(
      rep(list(statistic_spec), length(variables)),
      variables
    ))
  }

  unknown_variables <- setdiff(names(statistics), variables)
  if (length(unknown_variables) > 0) {
    stop(sprintf("Unknown weather variables in `statistics`: %s.", paste(unknown_variables, collapse = ", ")), call. = FALSE)
  }

  out <- list()
  for (variable in variables) {
    selected <- statistics[[variable]]
    if (is.null(selected)) {
      out[[variable]] <- list()
      next
    }
    out[[variable]] <- .resolve_statistic_spec(selected)
  }

  out
}

.resolve_statistic_spec <- function(statistics) {
  if (is.function(statistics)) {
    stop("Custom statistic functions must be supplied in a named list.", call. = FALSE)
  }

  if (is.character(statistics)) {
    unique_statistics <- unique(statistics)
    resolved <- lapply(unique_statistics, function(statistic) {
      tryCatch(
        match.fun(statistic),
        error = function(e) {
          stop(sprintf("Statistic function `%s` was not found.", statistic), call. = FALSE)
        }
      )
    })
    names(resolved) <- unique_statistics
    return(resolved)
  }

  if (!is.list(statistics) || length(statistics) == 0 || is.null(names(statistics)) || any(!nzchar(names(statistics)))) {
    stop("Custom statistic functions must be supplied in a named list.", call. = FALSE)
  }

  resolved <- lapply(seq_along(statistics), function(i) {
    statistic <- statistics[[i]]
    if (is.character(statistic) && length(statistic) == 1) {
      return(tryCatch(
        match.fun(statistic),
        error = function(e) {
          stop(sprintf("Statistic function `%s` was not found.", statistic), call. = FALSE)
        }
      ))
    }
    if (is.function(statistic)) {
      return(statistic)
    }
    stop("Each custom statistic must be a function or a single function name.", call. = FALSE)
  })
  names(resolved) <- names(statistics)
  resolved
}

.window_metric_names <- function(statistics) {
  unlist(lapply(names(statistics), function(variable) {
    paste(variable, names(statistics[[variable]]), sep = "_")
  }), use.names = FALSE)
}

.compute_window_statistic <- function(x, statistic_fun) {
  x <- x[!is.na(x)]
  if (length(x) == 0) {
    return(NA_real_)
  }

  value <- tryCatch(
    statistic_fun(x, na.rm = TRUE),
    error = function(e) statistic_fun(x)
  )
  if (!is.numeric(value) || length(value) != 1 || !is.finite(value)) {
    if (is.numeric(value) && length(value) == 1 && is.na(value)) {
      return(NA_real_)
    }
    stop("Each statistic function must return one numeric value.", call. = FALSE)
  }
  as.numeric(value)
}

