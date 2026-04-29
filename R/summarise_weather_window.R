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
#' @param statistics Summary statistics to compute. Use a character vector to
#'   apply the same statistics to all `weather_cols`, such as
#'   `c("mean", "sd", "IQR")`. Use a named list to choose statistics by
#'   variable or to provide custom named functions. Use `.conditions` for
#'   multivariable condition summaries such as
#'   `count_when(temp >= 18 & rh >= 90)`.
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
    statistics = list(
      temp = c("mean", "min", "max"),
      rh = "mean",
      rain = "sum",
      leaf_wetness = "sum"
  )
) {
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
  statistics <- .resolve_window_statistics(statistics, names(weather_cols))
  metric_names <- .window_metric_names(statistics)

  out <- data.frame(
    n_obs = nrow(weather),
    stringsAsFactors = FALSE
  )

  for (variable in names(statistics)) {
    for (statistic_name in names(statistics[[variable]])) {
      statistic_fun <- statistics[[variable]][[statistic_name]]
      if (identical(variable, ".conditions")) {
        out[[statistic_name]] <- .compute_condition_statistic(weather, statistic_fun)
      } else {
        out[[paste(variable, statistic_name, sep = "_")]] <- .compute_window_statistic(
          weather[[weather_cols[[variable]]]],
          statistic_fun
        )
      }
    }
  }

  out
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

.statistics_condition_only <- function(statistics) {
  is.list(statistics) &&
    !is.null(names(statistics)) &&
    length(statistics) == 1 &&
    identical(names(statistics), ".conditions")
}

.extend_weather_cols_from_statistics <- function(
    weather_cols,
    statistics,
    data_names,
    replace_defaults = FALSE
) {
  if (!is.list(statistics) || is.null(names(statistics))) {
    return(weather_cols)
  }

  statistic_variables <- setdiff(names(statistics), ".conditions")
  statistic_variables_in_data <- statistic_variables[statistic_variables %in% data_names]
  if (isTRUE(replace_defaults) && length(statistic_variables_in_data) > 0) {
    return(stats::setNames(statistic_variables_in_data, statistic_variables_in_data))
  }

  extra_variables <- setdiff(statistic_variables, names(weather_cols))
  extra_variables <- extra_variables[extra_variables %in% data_names]
  if (length(extra_variables) == 0) {
    return(weather_cols)
  }

  c(weather_cols, stats::setNames(extra_variables, extra_variables))
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

  if (!any(names(statistics) %in% variables) && !".conditions" %in% names(statistics)) {
    statistic_spec <- .resolve_statistic_spec(statistics)
    return(stats::setNames(
      rep(list(statistic_spec), length(variables)),
      variables
    ))
  }

  unknown_variables <- setdiff(names(statistics), c(variables, ".conditions"))
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
  if (".conditions" %in% names(statistics)) {
    out[[".conditions"]] <- .resolve_condition_statistic_spec(statistics[[".conditions"]])
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
    if (identical(variable, ".conditions")) {
      return(names(statistics[[variable]]))
    }
    paste(variable, names(statistics[[variable]]), sep = "_")
  }), use.names = FALSE)
}

.compute_window_statistic <- function(x, statistic_fun) {
  value <- tryCatch(
    suppressWarnings(statistic_fun(x, na.rm = TRUE)),
    error = function(e) statistic_fun(x)
  )
  if (is.numeric(value) && length(value) == 1 && is.infinite(value)) {
    return(NA_real_)
  }
  if (!is.numeric(value) || length(value) != 1 || !is.finite(value)) {
    if (is.numeric(value) && length(value) == 1 && is.na(value)) {
      return(NA_real_)
    }
    stop("Each statistic function must return one numeric value.", call. = FALSE)
  }
  as.numeric(value)
}

.resolve_condition_statistic_spec <- function(statistics) {
  if (!is.list(statistics) || length(statistics) == 0 || is.null(names(statistics)) || any(!nzchar(names(statistics)))) {
    stop("`.conditions` must be a named list of condition summary functions.", call. = FALSE)
  }
  resolved <- lapply(seq_along(statistics), function(i) {
    statistic <- statistics[[i]]
    if (!.is_condition_summary(statistic)) {
      stop("Each `.conditions` entry must be created by a condition summary function such as `count_when()`.", call. = FALSE)
    }
    statistic
  })
  names(resolved) <- names(statistics)
  resolved
}

.compute_condition_statistic <- function(weather, statistic_fun) {
  value <- statistic_fun(weather)
  if (!is.numeric(value) || length(value) != 1 || !is.finite(value)) {
    if (is.numeric(value) && length(value) == 1 && is.na(value)) {
      return(NA_real_)
    }
    stop("Each condition summary function must return one numeric value.", call. = FALSE)
  }
  as.numeric(value)
}

