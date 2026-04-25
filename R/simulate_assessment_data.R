#' Simulate One Disease Assessment per Weather Series
#'
#' Creates one synthetic disease assessment for each weather time series. In the
#' usual use case, each time series represents one location, plot, or
#' experimental unit, and the assessment is measured once at the end of that
#' series. The simulated response can be binary, ordinal, or a 0--100 percent
#' severity value.
#'
#' Disease risk is generated from interpretable weather summaries over each
#' series: cumulative rain, mean relative humidity, mean temperature, and the
#' proportion of wet observations. The result is intended for examples,
#' tutorials, and tests rather than biological inference.
#'
#' @param weather A weather data frame with at least the standard windcut
#'   columns.
#' @param id_col Optional column identifying independent weather series, such as
#'   locations. If `NULL`, the whole weather table is treated as one series.
#' @param response_type Type of disease response to simulate. One of
#'   `"binary"`, `"ordinal"`, or `"percent"`.
#' @param n_levels Number of ordinal classes when `response_type = "ordinal"`.
#'   For example, `n_levels = 10` returns scores from 0 to 9.
#' @param seed Optional random seed for reproducibility.
#' @param id_prefix Prefix used to label assessments when `id_col = NULL`.
#' @param time_col Name of the weather time column.
#' @param response_col Name of the simulated response column.
#'
#' @return A data frame with one row per weather series. It includes the series
#'   identifier, assessment time, response type, and simulated disease response.
#'
#' @examples
#' weather <- simulate_weather_series(days = 30, seed = 1)
#' simulate_assessment_data(weather, response_type = "binary", seed = 1)
#'
#' site_weather <- simulate_weather_series(
#'   days = 30,
#'   n_series = 4,
#'   id_col = "site_id",
#'   seed = 1
#' )
#'
#' simulate_assessment_data(
#'   site_weather,
#'   id_col = "site_id",
#'   response_type = "ordinal",
#'   n_levels = 10,
#'   seed = 1
#' )
#'
#' @export
simulate_assessment_data <- function(
    weather,
    id_col = NULL,
    response_type = c("percent", "binary", "ordinal"),
    n_levels = 10,
    seed = NULL,
    id_prefix = "A",
    time_col = "time",
    response_col = "disease_intensity"
) {
  response_type <- match.arg(response_type)

  if (!is.null(seed)) {
    set.seed(seed)
  }

  if (!is.null(id_col) && !id_col %in% names(weather)) {
    stop(sprintf("`id_col` '%s' was not found.", id_col), call. = FALSE)
  }

  if (!is.numeric(n_levels) || length(n_levels) != 1 || n_levels < 2) {
    stop("`n_levels` must be a single number greater than or equal to 2.", call. = FALSE)
  }

  n_levels <- as.integer(n_levels)
  if (is.null(id_col)) {
    weather <- validate_weather_data(weather, time_col = time_col)
    weather$.series_id <- sprintf("%s01", id_prefix)
    series_col <- ".series_id"
  } else {
    split_weather <- split(weather, weather[[id_col]])
    weather <- do.call(rbind, lapply(names(split_weather), function(series_id) {
      series <- validate_weather_data(split_weather[[series_id]], time_col = time_col)
      series[[id_col]] <- series_id
      series
    }))
    rownames(weather) <- NULL
    series_col <- id_col
  }

  series_ids <- unique(weather[[series_col]])
  rows <- lapply(series_ids, function(series_id) {
    series <- weather[weather[[series_col]] == series_id, , drop = FALSE]
    risk <- .simulate_disease_risk(series)
    value <- .simulate_disease_response(risk, response_type, n_levels)

    out <- data.frame(
      assessment_id = as.character(series_id),
      assessment_time = max(series[[time_col]]),
      response_type = response_type,
      stringsAsFactors = FALSE
    )

    if (!is.null(id_col)) {
      out[[id_col]] <- series_id
      out <- out[, c(id_col, setdiff(names(out), id_col)), drop = FALSE]
    }

    out[[response_col]] <- value
    out
  })

  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

.simulate_disease_risk <- function(series) {
  rain_signal <- log1p(sum(series$rain, na.rm = TRUE))
  humidity_signal <- (mean(series$rh, na.rm = TRUE) - 75) / 10
  wetness_signal <- mean(series$leaf_wetness, na.rm = TRUE) * 4
  temp_signal <- -abs(mean(series$temp, na.rm = TRUE) - 24) / 8

  linear_predictor <- -2.2 +
    0.35 * rain_signal +
    0.45 * humidity_signal +
    0.70 * wetness_signal +
    0.60 * temp_signal +
    stats::rnorm(1, sd = 0.35)

  stats::plogis(linear_predictor)
}

.simulate_disease_response <- function(risk, response_type, n_levels) {
  if (identical(response_type, "binary")) {
    return(stats::rbinom(1, size = 1, prob = risk))
  }

  if (identical(response_type, "ordinal")) {
    score <- round(risk * (n_levels - 1) + stats::rnorm(1, sd = 0.6))
    return(max(0, min(n_levels - 1, score)))
  }

  severity <- 100 * risk + stats::rnorm(1, sd = 6)
  round(max(0, min(100, severity)), 1)
}

