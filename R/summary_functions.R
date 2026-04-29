#' Threshold-based summary functions
#'
#' These functions create summary functions for use inside the `statistics`
#' argument of [summarise_weather_window()], [scan_windows()], and
#' [window_pane()]. Each function returns a closure that is applied to a numeric
#' vector from one weather variable.
#'
#' @param threshold A single finite numeric threshold.
#' @param lower,upper Single finite numeric limits. `lower` must be less than or
#'   equal to `upper`.
#' @param inclusive If `TRUE`, values equal to the boundary are included.
#' @param na.rm If `TRUE`, missing values are ignored. For proportion functions,
#'   the denominator is the number of non-missing observations.
#'
#' @return A function that takes a numeric vector and returns one numeric value.
#'
#' @examples
#' count_above(30)(c(28, 31, 33, NA))
#' count_between(18, 26)(c(17, 18, 22, 27))
#' proportion_at_or_above(90)(c(88, 91, 95, NA))
#'
#' statistics <- list(
#'   temp = list(hours_18_26 = count_between(18, 26)),
#'   rh = list(prop_humid = proportion_at_or_above(90))
#' )
#'
#' @name threshold_summary_functions
NULL

#' @rdname threshold_summary_functions
#' @export
count_above <- function(threshold, na.rm = TRUE) {
  .threshold_count(threshold, ">", na.rm)
}

#' @rdname threshold_summary_functions
#' @export
count_at_or_above <- function(threshold, na.rm = TRUE) {
  .threshold_count(threshold, ">=", na.rm)
}

#' @rdname threshold_summary_functions
#' @export
count_below <- function(threshold, na.rm = TRUE) {
  .threshold_count(threshold, "<", na.rm)
}

#' @rdname threshold_summary_functions
#' @export
count_at_or_below <- function(threshold, na.rm = TRUE) {
  .threshold_count(threshold, "<=", na.rm)
}

#' @rdname threshold_summary_functions
#' @export
count_between <- function(lower, upper, inclusive = TRUE, na.rm = TRUE) {
  .between_count(lower, upper, inclusive, na.rm)
}

#' @rdname threshold_summary_functions
#' @export
proportion_above <- function(threshold, na.rm = TRUE) {
  .threshold_proportion(threshold, ">", na.rm)
}

#' @rdname threshold_summary_functions
#' @export
proportion_at_or_above <- function(threshold, na.rm = TRUE) {
  .threshold_proportion(threshold, ">=", na.rm)
}

#' @rdname threshold_summary_functions
#' @export
proportion_below <- function(threshold, na.rm = TRUE) {
  .threshold_proportion(threshold, "<", na.rm)
}

#' @rdname threshold_summary_functions
#' @export
proportion_at_or_below <- function(threshold, na.rm = TRUE) {
  .threshold_proportion(threshold, "<=", na.rm)
}

#' @rdname threshold_summary_functions
#' @export
proportion_between <- function(lower, upper, inclusive = TRUE, na.rm = TRUE) {
  .between_proportion(lower, upper, inclusive, na.rm)
}

#' Conditional value summary functions
#'
#' These functions summarize only values that satisfy a threshold condition.
#' `sum_*()` returns zero when valid observations exist but none satisfy the
#' condition. `mean_*()` returns `NA_real_` when no value satisfies the
#' condition.
#'
#' @inheritParams threshold_summary_functions
#'
#' @return A function that takes a numeric vector and returns one numeric value.
#'
#' @examples
#' sum_above(0)(c(0, 1.2, 3.4, NA))
#' mean_between(18, 26)(c(14, 20, 22, 30))
#'
#' @name conditional_value_summary_functions
NULL

#' @rdname conditional_value_summary_functions
#' @export
sum_above <- function(threshold, na.rm = TRUE) {
  .threshold_value_summary(threshold, ">", "sum", na.rm)
}

#' @rdname conditional_value_summary_functions
#' @export
sum_at_or_above <- function(threshold, na.rm = TRUE) {
  .threshold_value_summary(threshold, ">=", "sum", na.rm)
}

#' @rdname conditional_value_summary_functions
#' @export
sum_below <- function(threshold, na.rm = TRUE) {
  .threshold_value_summary(threshold, "<", "sum", na.rm)
}

#' @rdname conditional_value_summary_functions
#' @export
sum_between <- function(lower, upper, inclusive = TRUE, na.rm = TRUE) {
  .between_value_summary(lower, upper, inclusive, "sum", na.rm)
}

#' @rdname conditional_value_summary_functions
#' @export
mean_above <- function(threshold, na.rm = TRUE) {
  .threshold_value_summary(threshold, ">", "mean", na.rm)
}

#' @rdname conditional_value_summary_functions
#' @export
mean_at_or_above <- function(threshold, na.rm = TRUE) {
  .threshold_value_summary(threshold, ">=", "mean", na.rm)
}

#' @rdname conditional_value_summary_functions
#' @export
mean_below <- function(threshold, na.rm = TRUE) {
  .threshold_value_summary(threshold, "<", "mean", na.rm)
}

#' @rdname conditional_value_summary_functions
#' @export
mean_between <- function(lower, upper, inclusive = TRUE, na.rm = TRUE) {
  .between_value_summary(lower, upper, inclusive, "mean", na.rm)
}

#' Thermal-time summary functions
#'
#' These functions summarize heat accumulation from temperature vectors.
#' `degree_days_*()` divides degree-hours by 24 and therefore assumes hourly
#' input data. For daily data, use `degree_hours_*()` only if each row already
#' represents the desired time step.
#'
#' @param base A single finite numeric base temperature.
#' @inheritParams threshold_summary_functions
#'
#' @return A function that takes a numeric vector and returns one numeric value.
#'
#' @examples
#' degree_hours_above(10)(c(8, 10, 12, 15))
#' thermal_time_between(18, 26)(c(16, 20, 30))
#'
#' @name thermal_time_functions
NULL

#' @rdname thermal_time_functions
#' @export
degree_hours_above <- function(base, na.rm = TRUE) {
  .validate_numeric_scalar(base, "base")
  .validate_flag(na.rm, "na.rm")
  force(base)
  force(na.rm)
  function(x) {
    x <- .valid_values(x, na.rm = na.rm)
    if (is.null(x)) {
      return(NA_real_)
    }
    sum(pmax(x - base, 0), na.rm = na.rm)
  }
}

#' @rdname thermal_time_functions
#' @export
degree_hours_below <- function(base, na.rm = TRUE) {
  .validate_numeric_scalar(base, "base")
  .validate_flag(na.rm, "na.rm")
  force(base)
  force(na.rm)
  function(x) {
    x <- .valid_values(x, na.rm = na.rm)
    if (is.null(x)) {
      return(NA_real_)
    }
    sum(pmax(base - x, 0), na.rm = na.rm)
  }
}

#' @rdname thermal_time_functions
#' @export
degree_days_above <- function(base, na.rm = TRUE) {
  fun <- degree_hours_above(base, na.rm = na.rm)
  function(x) fun(x) / 24
}

#' @rdname thermal_time_functions
#' @export
degree_days_below <- function(base, na.rm = TRUE) {
  fun <- degree_hours_below(base, na.rm = na.rm)
  function(x) fun(x) / 24
}

#' @rdname thermal_time_functions
#' @export
thermal_time_above <- function(base, na.rm = TRUE) {
  degree_hours_above(base, na.rm = na.rm)
}

#' @rdname thermal_time_functions
#' @export
thermal_time_between <- function(lower, upper, na.rm = TRUE) {
  .validate_numeric_pair(lower, upper)
  .validate_flag(na.rm, "na.rm")
  force(lower)
  force(upper)
  force(na.rm)
  function(x) {
    x <- .valid_values(x, na.rm = na.rm)
    if (is.null(x)) {
      return(NA_real_)
    }
    sum(pmax(pmin(x, upper) - lower, 0), na.rm = na.rm)
  }
}

#' @rdname thermal_time_functions
#' @export
hours_in_temperature_range <- function(lower, upper, inclusive = TRUE, na.rm = TRUE) {
  count_between(lower, upper, inclusive = inclusive, na.rm = na.rm)
}

#' @rdname thermal_time_functions
#' @export
proportion_in_temperature_range <- function(lower, upper, inclusive = TRUE, na.rm = TRUE) {
  proportion_between(lower, upper, inclusive = inclusive, na.rm = na.rm)
}

#' Weather-specific summary functions
#'
#' These functions provide readable names for common epidemiological summaries.
#' The units of names such as `rainy_days()` and `wet_days()` depend on the time
#' resolution of the input data. With hourly data they count hours; with daily
#' data they count days.
#'
#' @inheritParams threshold_summary_functions
#'
#' @return A function that takes a numeric vector and returns one numeric value.
#'
#' @examples
#' humid_hours()(c(85, 91, 96))
#' rain_events(0.2)(c(0, 0.3, 0.4, 0, 1.2))
#'
#' @name weather_specific_summary_functions
NULL

#' @rdname weather_specific_summary_functions
#' @export
humid_hours <- function(threshold = 90, na.rm = TRUE) {
  count_at_or_above(threshold, na.rm = na.rm)
}

#' @rdname weather_specific_summary_functions
#' @export
dry_hours <- function(threshold = 60, na.rm = TRUE) {
  count_at_or_below(threshold, na.rm = na.rm)
}

#' @rdname weather_specific_summary_functions
#' @export
proportion_humid <- function(threshold = 90, na.rm = TRUE) {
  proportion_at_or_above(threshold, na.rm = na.rm)
}

#' @rdname weather_specific_summary_functions
#' @export
proportion_dry <- function(threshold = 60, na.rm = TRUE) {
  proportion_at_or_below(threshold, na.rm = na.rm)
}

#' @rdname weather_specific_summary_functions
#' @export
max_consecutive_humid_hours <- function(threshold = 90, na.rm = TRUE) {
  max_consecutive_at_or_above(threshold, na.rm = na.rm)
}

#' @rdname weather_specific_summary_functions
#' @export
max_consecutive_dry_hours <- function(threshold = 60, na.rm = TRUE) {
  max_consecutive_at_or_below(threshold, na.rm = na.rm)
}

#' @rdname weather_specific_summary_functions
#' @export
rainy_hours <- function(threshold = 0, na.rm = TRUE) {
  count_above(threshold, na.rm = na.rm)
}

#' @rdname weather_specific_summary_functions
#' @export
rainy_days <- function(threshold = 0, na.rm = TRUE) {
  rainy_hours(threshold, na.rm = na.rm)
}

#' @rdname weather_specific_summary_functions
#' @export
rain_events <- function(threshold = 0.2, na.rm = TRUE) {
  spell_count_above(threshold, na.rm = na.rm)
}

#' @rdname weather_specific_summary_functions
#' @export
rain_event_count <- function(threshold = 0.2, na.rm = TRUE) {
  rain_events(threshold, na.rm = na.rm)
}

#' @rdname weather_specific_summary_functions
#' @export
max_rain_event <- function(threshold = 0.2, na.rm = TRUE) {
  .validate_numeric_scalar(threshold, "threshold")
  .validate_flag(na.rm, "na.rm")
  force(threshold)
  force(na.rm)
  function(x) {
    sums <- .event_sums(x, x > threshold, na.rm = na.rm)
    if (length(sums) == 0) {
      return(0)
    }
    max(sums)
  }
}

#' @rdname weather_specific_summary_functions
#' @export
mean_rain_event <- function(threshold = 0.2, na.rm = TRUE) {
  .validate_numeric_scalar(threshold, "threshold")
  .validate_flag(na.rm, "na.rm")
  force(threshold)
  force(na.rm)
  function(x) {
    sums <- .event_sums(x, x > threshold, na.rm = na.rm)
    if (length(sums) == 0) {
      return(NA_real_)
    }
    mean(sums)
  }
}

#' @rdname weather_specific_summary_functions
#' @export
hours_since_last_rain <- function(threshold = 0.2, na.rm = TRUE) {
  .validate_numeric_scalar(threshold, "threshold")
  .validate_flag(na.rm, "na.rm")
  force(threshold)
  force(na.rm)
  function(x) {
    x <- .valid_values(x, na.rm = na.rm)
    if (is.null(x)) {
      return(NA_real_)
    }
    rainy <- x > threshold
    if (isTRUE(na.rm)) {
      rainy[is.na(rainy)] <- FALSE
    } else if (any(is.na(rainy))) {
      return(NA_real_)
    }
    last <- utils::tail(which(rainy), 1)
    if (length(last) == 0) {
      return(NA_real_)
    }
    length(x) - last
  }
}

#' @rdname weather_specific_summary_functions
#' @export
days_since_last_rain <- function(threshold = 0.2, na.rm = TRUE) {
  hours_since_last_rain(threshold, na.rm = na.rm)
}

#' @rdname weather_specific_summary_functions
#' @export
wet_hours <- function(threshold = 0, na.rm = TRUE) {
  count_above(threshold, na.rm = na.rm)
}

#' @rdname weather_specific_summary_functions
#' @export
wet_days <- function(threshold = 0, na.rm = TRUE) {
  wet_hours(threshold, na.rm = na.rm)
}

#' @rdname weather_specific_summary_functions
#' @export
proportion_wet <- function(threshold = 0, na.rm = TRUE) {
  proportion_above(threshold, na.rm = na.rm)
}

#' @rdname weather_specific_summary_functions
#' @export
max_consecutive_wet_hours <- function(threshold = 0, na.rm = TRUE) {
  max_consecutive_above(threshold, na.rm = na.rm)
}

#' @rdname weather_specific_summary_functions
#' @export
wet_spell_count <- function(threshold = 0, na.rm = TRUE) {
  spell_count_above(threshold, na.rm = na.rm)
}

#' @rdname weather_specific_summary_functions
#' @export
mean_wet_spell_duration <- function(threshold = 0, na.rm = TRUE) {
  mean_spell_duration_above(threshold, na.rm = na.rm)
}

#' @rdname weather_specific_summary_functions
#' @export
max_wet_spell_duration <- function(threshold = 0, na.rm = TRUE) {
  max_spell_duration_above(threshold, na.rm = na.rm)
}

#' Consecutive spell summary functions
#'
#' These functions summarize runs of consecutive observations that satisfy a
#' condition. Missing values break sequences when `na.rm = TRUE`.
#'
#' @inheritParams threshold_summary_functions
#'
#' @return A function that takes a numeric vector and returns one numeric value.
#'
#' @examples
#' max_consecutive_above(2)(c(1, 3, 4, 1, 5, 6, 7))
#' spell_count_above(2)(c(1, 3, 4, 1, 5, 6, 7))
#'
#' @name spell_summary_functions
NULL

#' @rdname spell_summary_functions
#' @export
max_consecutive_above <- function(threshold, na.rm = TRUE) {
  .threshold_run_summary(threshold, ">", "max", na.rm)
}

#' @rdname spell_summary_functions
#' @export
max_consecutive_at_or_above <- function(threshold, na.rm = TRUE) {
  .threshold_run_summary(threshold, ">=", "max", na.rm)
}

#' @rdname spell_summary_functions
#' @export
max_consecutive_below <- function(threshold, na.rm = TRUE) {
  .threshold_run_summary(threshold, "<", "max", na.rm)
}

#' @rdname spell_summary_functions
#' @export
max_consecutive_at_or_below <- function(threshold, na.rm = TRUE) {
  .threshold_run_summary(threshold, "<=", "max", na.rm)
}

#' @rdname spell_summary_functions
#' @export
max_consecutive_between <- function(lower, upper, inclusive = TRUE, na.rm = TRUE) {
  .between_run_summary(lower, upper, inclusive, "max", na.rm)
}

#' @rdname spell_summary_functions
#' @export
spell_count_above <- function(threshold, na.rm = TRUE) {
  .threshold_run_summary(threshold, ">", "count", na.rm)
}

#' @rdname spell_summary_functions
#' @export
spell_count_at_or_above <- function(threshold, na.rm = TRUE) {
  .threshold_run_summary(threshold, ">=", "count", na.rm)
}

#' @rdname spell_summary_functions
#' @export
spell_count_below <- function(threshold, na.rm = TRUE) {
  .threshold_run_summary(threshold, "<", "count", na.rm)
}

#' @rdname spell_summary_functions
#' @export
spell_count_between <- function(lower, upper, inclusive = TRUE, na.rm = TRUE) {
  .between_run_summary(lower, upper, inclusive, "count", na.rm)
}

#' @rdname spell_summary_functions
#' @export
mean_spell_duration_above <- function(threshold, na.rm = TRUE) {
  .threshold_run_summary(threshold, ">", "mean", na.rm)
}

#' @rdname spell_summary_functions
#' @export
max_spell_duration_above <- function(threshold, na.rm = TRUE) {
  max_consecutive_above(threshold, na.rm = na.rm)
}

#' Multivariable condition summary functions
#'
#' These functions create summaries from expressions evaluated in a weather
#' window data frame. They are used inside the special `.conditions` entry of
#' `statistics`.
#'
#' @param condition An expression evaluated in the window data frame.
#' @param variable A numeric column evaluated in the window data frame.
#' @inheritParams threshold_summary_functions
#'
#' @return A function marked as a windcut condition summary. It receives a data
#'   frame and returns one numeric value.
#'
#' @examples
#' weather <- data.frame(temp = c(17, 20, 24), rh = c(88, 92, 95), rain = c(0, 1, 0))
#' count_when(temp >= 18 & temp <= 26 & rh >= 90)(weather)
#'
#' statistics <- list(
#'   temp = list(mean = "mean"),
#'   .conditions = list(
#'     favorable_hours = count_when(temp >= 18 & temp <= 26 & rh >= 90)
#'   )
#' )
#'
#' @name multivariable_condition_summary_functions
NULL

#' @rdname multivariable_condition_summary_functions
#' @export
count_when <- function(condition, na.rm = TRUE) {
  condition_expr <- substitute(condition)
  .condition_summary(function(data) {
    condition <- .eval_condition(condition_expr, data)
    .count_logical(condition, na.rm = na.rm)
  })
}

#' @rdname multivariable_condition_summary_functions
#' @export
proportion_when <- function(condition, na.rm = TRUE) {
  condition_expr <- substitute(condition)
  .condition_summary(function(data) {
    condition <- .eval_condition(condition_expr, data)
    .proportion_logical(condition, na.rm = na.rm)
  })
}

#' @rdname multivariable_condition_summary_functions
#' @export
sum_when <- function(variable, condition, na.rm = TRUE) {
  variable_expr <- substitute(variable)
  condition_expr <- substitute(condition)
  .condition_summary(function(data) {
    x <- .eval_numeric(variable_expr, data)
    condition <- .eval_condition(condition_expr, data)
    .conditional_numeric_summary(x, condition, "sum", na.rm = na.rm)
  })
}

#' @rdname multivariable_condition_summary_functions
#' @export
mean_when <- function(variable, condition, na.rm = TRUE) {
  variable_expr <- substitute(variable)
  condition_expr <- substitute(condition)
  .condition_summary(function(data) {
    x <- .eval_numeric(variable_expr, data)
    condition <- .eval_condition(condition_expr, data)
    .conditional_numeric_summary(x, condition, "mean", na.rm = na.rm)
  })
}

#' @rdname multivariable_condition_summary_functions
#' @export
max_when <- function(variable, condition, na.rm = TRUE) {
  variable_expr <- substitute(variable)
  condition_expr <- substitute(condition)
  .condition_summary(function(data) {
    x <- .eval_numeric(variable_expr, data)
    condition <- .eval_condition(condition_expr, data)
    .conditional_numeric_summary(x, condition, "max", na.rm = na.rm)
  })
}

#' @rdname multivariable_condition_summary_functions
#' @export
min_when <- function(variable, condition, na.rm = TRUE) {
  variable_expr <- substitute(variable)
  condition_expr <- substitute(condition)
  .condition_summary(function(data) {
    x <- .eval_numeric(variable_expr, data)
    condition <- .eval_condition(condition_expr, data)
    .conditional_numeric_summary(x, condition, "min", na.rm = na.rm)
  })
}

#' @rdname multivariable_condition_summary_functions
#' @export
max_consecutive_when <- function(condition, na.rm = TRUE) {
  condition_expr <- substitute(condition)
  .condition_summary(function(data) {
    condition <- .eval_condition(condition_expr, data)
    lengths <- .run_lengths(condition, na.rm = na.rm)
    if (length(lengths) == 0) 0L else as.integer(max(lengths))
  })
}

#' @rdname multivariable_condition_summary_functions
#' @export
spell_count_when <- function(condition, na.rm = TRUE) {
  condition_expr <- substitute(condition)
  .condition_summary(function(data) {
    condition <- .eval_condition(condition_expr, data)
    as.integer(length(.run_lengths(condition, na.rm = na.rm)))
  })
}

#' @rdname multivariable_condition_summary_functions
#' @export
mean_spell_duration_when <- function(condition, na.rm = TRUE) {
  condition_expr <- substitute(condition)
  .condition_summary(function(data) {
    condition <- .eval_condition(condition_expr, data)
    lengths <- .run_lengths(condition, na.rm = na.rm)
    if (length(lengths) == 0) NA_real_ else mean(lengths)
  })
}

#' @rdname multivariable_condition_summary_functions
#' @export
max_spell_duration_when <- function(condition, na.rm = TRUE) {
  condition_expr <- substitute(condition)
  .condition_summary(function(data) {
    condition <- .eval_condition(condition_expr, data)
    lengths <- .run_lengths(condition, na.rm = na.rm)
    if (length(lengths) == 0) 0L else as.integer(max(lengths))
  })
}

.threshold_count <- function(threshold, operator, na.rm) {
  .validate_numeric_scalar(threshold, "threshold")
  .validate_flag(na.rm, "na.rm")
  force(threshold)
  force(operator)
  force(na.rm)
  function(x) {
    condition <- .condition_vector(x, threshold, operator)
    .count_logical(condition, na.rm = na.rm)
  }
}

.between_count <- function(lower, upper, inclusive, na.rm) {
  .validate_numeric_pair(lower, upper)
  .validate_flag(inclusive, "inclusive")
  .validate_flag(na.rm, "na.rm")
  force(lower)
  force(upper)
  force(inclusive)
  force(na.rm)
  function(x) {
    condition <- .between_condition(x, lower, upper, inclusive)
    .count_logical(condition, na.rm = na.rm)
  }
}

.threshold_proportion <- function(threshold, operator, na.rm) {
  .validate_numeric_scalar(threshold, "threshold")
  .validate_flag(na.rm, "na.rm")
  force(threshold)
  force(operator)
  force(na.rm)
  function(x) {
    condition <- .condition_vector(x, threshold, operator)
    .proportion_logical(condition, na.rm = na.rm)
  }
}

.between_proportion <- function(lower, upper, inclusive, na.rm) {
  .validate_numeric_pair(lower, upper)
  .validate_flag(inclusive, "inclusive")
  .validate_flag(na.rm, "na.rm")
  force(lower)
  force(upper)
  force(inclusive)
  force(na.rm)
  function(x) {
    condition <- .between_condition(x, lower, upper, inclusive)
    .proportion_logical(condition, na.rm = na.rm)
  }
}

.threshold_value_summary <- function(threshold, operator, summary, na.rm) {
  .validate_numeric_scalar(threshold, "threshold")
  .validate_flag(na.rm, "na.rm")
  force(threshold)
  force(operator)
  force(summary)
  force(na.rm)
  function(x) {
    condition <- .condition_vector(x, threshold, operator)
    .conditional_numeric_summary(x, condition, summary, na.rm = na.rm)
  }
}

.between_value_summary <- function(lower, upper, inclusive, summary, na.rm) {
  .validate_numeric_pair(lower, upper)
  .validate_flag(inclusive, "inclusive")
  .validate_flag(na.rm, "na.rm")
  force(lower)
  force(upper)
  force(inclusive)
  force(summary)
  force(na.rm)
  function(x) {
    condition <- .between_condition(x, lower, upper, inclusive)
    .conditional_numeric_summary(x, condition, summary, na.rm = na.rm)
  }
}

.threshold_run_summary <- function(threshold, operator, summary, na.rm) {
  .validate_numeric_scalar(threshold, "threshold")
  .validate_flag(na.rm, "na.rm")
  force(threshold)
  force(operator)
  force(summary)
  force(na.rm)
  function(x) {
    condition <- .condition_vector(x, threshold, operator)
    .run_summary(condition, summary, na.rm = na.rm)
  }
}

.between_run_summary <- function(lower, upper, inclusive, summary, na.rm) {
  .validate_numeric_pair(lower, upper)
  .validate_flag(inclusive, "inclusive")
  .validate_flag(na.rm, "na.rm")
  force(lower)
  force(upper)
  force(inclusive)
  force(summary)
  force(na.rm)
  function(x) {
    condition <- .between_condition(x, lower, upper, inclusive)
    .run_summary(condition, summary, na.rm = na.rm)
  }
}

.validate_numeric_scalar <- function(x, name) {
  if (!is.numeric(x) || length(x) != 1 || is.na(x) || !is.finite(x)) {
    stop(sprintf("`%s` must be a single finite number.", name), call. = FALSE)
  }
  invisible(x)
}

.validate_numeric_pair <- function(lower, upper) {
  .validate_numeric_scalar(lower, "lower")
  .validate_numeric_scalar(upper, "upper")
  if (lower > upper) {
    stop("`lower` must be less than or equal to `upper`.", call. = FALSE)
  }
  invisible(TRUE)
}

.validate_flag <- function(x, name) {
  if (!is.logical(x) || length(x) != 1 || is.na(x)) {
    stop(sprintf("`%s` must be `TRUE` or `FALSE`.", name), call. = FALSE)
  }
  invisible(x)
}

.valid_values <- function(x, na.rm = TRUE) {
  if (!is.numeric(x)) {
    stop("Summary functions require numeric vectors.", call. = FALSE)
  }
  if (length(x) == 0) {
    return(NULL)
  }
  if (isTRUE(na.rm)) {
    x <- x[!is.na(x)]
  }
  if (length(x) == 0) {
    return(NULL)
  }
  x
}

.condition_vector <- function(x, threshold, operator) {
  if (!is.numeric(x)) {
    stop("Summary functions require numeric vectors.", call. = FALSE)
  }
  switch(
    operator,
    ">" = x > threshold,
    ">=" = x >= threshold,
    "<" = x < threshold,
    "<=" = x <= threshold,
    stop("Unsupported comparison operator.", call. = FALSE)
  )
}

.between_condition <- function(x, lower, upper, inclusive) {
  if (!is.numeric(x)) {
    stop("Summary functions require numeric vectors.", call. = FALSE)
  }
  if (isTRUE(inclusive)) {
    x >= lower & x <= upper
  } else {
    x > lower & x < upper
  }
}

.count_logical <- function(condition, na.rm = TRUE) {
  if (!is.logical(condition)) {
    stop("Condition summaries require logical conditions.", call. = FALSE)
  }
  if (length(condition) == 0) {
    return(0L)
  }
  if (isTRUE(na.rm)) {
    return(as.integer(sum(condition, na.rm = TRUE)))
  }
  if (any(is.na(condition))) {
    return(NA_integer_)
  }
  as.integer(sum(condition))
}

.proportion_logical <- function(condition, na.rm = TRUE) {
  if (!is.logical(condition)) {
    stop("Condition summaries require logical conditions.", call. = FALSE)
  }
  if (length(condition) == 0) {
    return(NA_real_)
  }
  if (isTRUE(na.rm)) {
    valid <- !is.na(condition)
    if (!any(valid)) {
      return(NA_real_)
    }
    return(sum(condition[valid]) / sum(valid))
  }
  if (any(is.na(condition))) {
    return(NA_real_)
  }
  mean(condition)
}

.conditional_numeric_summary <- function(x, condition, summary, na.rm = TRUE) {
  if (!is.numeric(x)) {
    stop("Value summaries require numeric vectors.", call. = FALSE)
  }
  if (!is.logical(condition) || length(condition) != length(x)) {
    stop("`condition` must be a logical vector with the same length as the numeric variable.", call. = FALSE)
  }
  if (length(x) == 0) {
    return(NA_real_)
  }
  if (isTRUE(na.rm)) {
    valid_data <- !is.na(x)
    if (!any(valid_data)) {
      return(NA_real_)
    }
    selected <- condition & valid_data
    selected[is.na(selected)] <- FALSE
  } else {
    if (any(is.na(condition)) || any(is.na(x))) {
      return(NA_real_)
    }
    selected <- condition
  }
  values <- x[selected]
  if (length(values) == 0) {
    if (identical(summary, "sum")) {
      return(0)
    }
    return(NA_real_)
  }
  switch(
    summary,
    sum = sum(values),
    mean = mean(values),
    max = max(values),
    min = min(values),
    stop("Unsupported numeric summary.", call. = FALSE)
  )
}

.run_lengths <- function(condition, na.rm = TRUE) {
  if (!is.logical(condition)) {
    stop("Spell summaries require logical conditions.", call. = FALSE)
  }
  if (length(condition) == 0) {
    return(integer())
  }
  if (isTRUE(na.rm)) {
    condition[is.na(condition)] <- FALSE
  } else if (any(is.na(condition))) {
    return(NA_integer_)
  }
  runs <- rle(condition)
  as.integer(runs$lengths[runs$values])
}

.run_summary <- function(condition, summary, na.rm = TRUE) {
  lengths <- .run_lengths(condition, na.rm = na.rm)
  if (length(lengths) == 1 && is.na(lengths)) {
    return(NA_real_)
  }
  switch(
    summary,
    count = as.integer(length(lengths)),
    max = if (length(lengths) == 0) 0L else as.integer(max(lengths)),
    mean = if (length(lengths) == 0) NA_real_ else mean(lengths),
    stop("Unsupported spell summary.", call. = FALSE)
  )
}

.event_sums <- function(x, condition, na.rm = TRUE) {
  if (!is.numeric(x)) {
    stop("Event summaries require numeric vectors.", call. = FALSE)
  }
  if (!is.logical(condition) || length(condition) != length(x)) {
    stop("`condition` must be a logical vector with the same length as the numeric variable.", call. = FALSE)
  }
  if (length(x) == 0) {
    return(numeric())
  }
  if (isTRUE(na.rm)) {
    condition[is.na(condition) | is.na(x)] <- FALSE
  } else if (any(is.na(condition)) || any(is.na(x))) {
    return(NA_real_)
  }
  runs <- rle(condition)
  ends <- cumsum(runs$lengths)
  starts <- ends - runs$lengths + 1
  event_ids <- which(runs$values)
  if (length(event_ids) == 0) {
    return(numeric())
  }
  vapply(event_ids, function(i) sum(x[starts[[i]]:ends[[i]]]), numeric(1))
}

.condition_summary <- function(fun) {
  class(fun) <- c("windcut_condition_summary", class(fun))
  fun
}

.is_condition_summary <- function(x) {
  is.function(x) && inherits(x, "windcut_condition_summary")
}

.eval_condition <- function(expr, data) {
  if (!is.data.frame(data)) {
    stop("Condition summaries require a data frame.", call. = FALSE)
  }
  value <- eval(expr, data, parent.frame())
  if (!is.logical(value) || length(value) != nrow(data)) {
    stop("`condition` must evaluate to one logical value per row.", call. = FALSE)
  }
  value
}

.eval_numeric <- function(expr, data) {
  if (!is.data.frame(data)) {
    stop("Condition summaries require a data frame.", call. = FALSE)
  }
  value <- eval(expr, data, parent.frame())
  if (!is.numeric(value) || length(value) != nrow(data)) {
    stop("`variable` must evaluate to one numeric value per row.", call. = FALSE)
  }
  value
}
