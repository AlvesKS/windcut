test_that("simulate_weather_series returns standard weather columns", {
  weather <- simulate_weather_series(days = 10, seed = 123)

  expect_equal(nrow(weather), 240)
  expect_true(all(c("time", "temp", "rh", "rain", "leaf_wetness") %in% names(weather)))
  expect_true(all(weather$rh >= 35 & weather$rh <= 100))
})

test_that("simulate_weather_series can generate multiple labeled series", {
  weather <- simulate_weather_series(
    days = 5,
    n_series = 4,
    id_col = "site_id",
    id_prefix = "S",
    seed = 123
  )

  expect_equal(nrow(weather), 4 * 5 * 24)
  expect_true(all(c("site_id", "time", "temp", "rh", "rain", "leaf_wetness") %in% names(weather)))
  expect_equal(length(unique(weather$site_id)), 4)
  expect_equal(unique(weather$site_id), sprintf("S%02d", 1:4))
})

test_that("simulate_weather_series uses a default series identifier for multiple series", {
  weather <- simulate_weather_series(days = 3, n_series = 2, seed = 99)

  expect_true("series_id" %in% names(weather))
  expect_equal(unique(weather$series_id), c("S01", "S02"))
})

test_that("default multi-series output works with simulate_assessment_data", {
  weather <- simulate_weather_series(days = 10, n_series = 3, seed = 15)
  assessments <- simulate_assessment_data(weather, id_col = "series_id", seed = 15)

  expect_equal(nrow(assessments), 3)
  expect_equal(assessments$series_id, c("S01", "S02", "S03"))
})

test_that("aggregate_weather_daily creates daily rows by series", {
  hourly <- simulate_weather_series(
    days = 4,
    n_series = 3,
    id_col = "site_id",
    seed = 2
  )

  daily <- aggregate_weather_daily(
    hourly,
    id_col = "site_id",
    statistics = list(
      temp = c("mean", "max"),
      rh = "mean",
      rain = "sum",
      leaf_wetness = "sum"
    )
  )

  expect_equal(nrow(daily), 12)
  expect_true(all(c("site_id", "date", "time", "daily_mean_temp", "daily_max_temp", "daily_mean_rh", "daily_sum_rain", "daily_sum_leaf_wetness") %in% names(daily)))
  expect_equal(length(unique(table(daily$site_id))), 1)
  expect_equal(unique(as.numeric(table(daily$site_id))), 4)
})

test_that("aggregate_weather_daily accepts arbitrary weather columns and statistics", {
  hourly <- simulate_weather_series(days = 2, seed = 3)
  names(hourly)[names(hourly) == "temp"] <- "temp2m"
  hourly$sradiation <- seq_len(nrow(hourly))

  daily <- aggregate_weather_daily(
    hourly,
    weather_cols = c(air_temp = "temp2m", solar = "sradiation"),
    statistics = list(
      air_temp = list(avg = "mean", p90 = function(x, na.rm = TRUE) stats::quantile(x, 0.9, na.rm = na.rm)),
      solar = "sum"
    )
  )

  expect_true(all(c("daily_avg_air_temp", "daily_p90_air_temp", "daily_sum_solar") %in% names(daily)))
  expect_equal(nrow(daily), 2)
})

test_that("aggregate_weather_daily can use original weather column names directly", {
  hourly <- simulate_weather_series(days = 2, seed = 31)
  names(hourly)[names(hourly) == "temp"] <- "temp2m"
  names(hourly)[names(hourly) == "rh"] <- "relhum"
  names(hourly)[names(hourly) == "rain"] <- "prectot"
  hourly$sradiation <- seq_len(nrow(hourly))

  daily <- aggregate_weather_daily(
    hourly,
    weather_cols = c("temp2m", "relhum", "prectot", "sradiation"),
    statistics = list(
      temp2m = c("mean", "max"),
      relhum = "mean",
      prectot = "sum",
      sradiation = "sum"
    )
  )

  expect_true(all(c("daily_mean_temp2m", "daily_max_temp2m", "daily_mean_relhum", "daily_sum_prectot", "daily_sum_sradiation") %in% names(daily)))
  expect_equal(nrow(daily), 2)
})

test_that("aggregate_weather_daily can use an existing date column", {
  hourly <- simulate_weather_series(days = 3, n_series = 2, id_col = "site_id", seed = 4)
  hourly$weather_day <- as.Date(hourly$time)

  daily <- aggregate_weather_daily(
    hourly,
    id_col = "site_id",
    time_col = "time",
    date_col = "weather_day",
    statistics = list(temp = "mean", rain = "sum")
  )

  expect_true("weather_day" %in% names(daily))
  expect_equal(nrow(daily), 6)
  expect_equal(unique(as.numeric(table(daily$site_id))), 3)
})

test_that("aggregate_weather_daily can aggregate already dated data without a time column", {
  daily_input <- data.frame(
    site_id = rep(c("S01", "S02"), each = 4),
    day = rep(as.Date("2024-01-01") + 0:1, each = 2, times = 2),
    temp = 1:8,
    rain = rep(c(0, 2), 4)
  )

  daily <- aggregate_weather_daily(
    daily_input,
    id_col = "site_id",
    time_col = NULL,
    date_col = "day",
    weather_cols = c("temp", "rain"),
    statistics = list(temp = "mean", rain = "sum"),
    keep_time = FALSE
  )

  expect_equal(nrow(daily), 4)
  expect_true(all(c("site_id", "day", "daily_mean_temp", "daily_sum_rain") %in% names(daily)))
  expect_false("time" %in% names(daily))
  expect_equal(daily$daily_mean_temp[daily$site_id == "S01" & daily$day == as.Date("2024-01-01")], 1.5)
  expect_equal(daily$daily_sum_rain[daily$site_id == "S01" & daily$day == as.Date("2024-01-01")], 2)
})

test_that("bundled demo datasets have expected structure", {
  data(window_pane_demo_data, package = "windcut")
  data(fda_demo_data, package = "windcut")

  expect_true(all(c("weather", "weather_hourly", "assessments") %in% names(window_pane_demo_data)))
  expect_true(all(c("site_id", "time", "daily_mean_temp", "daily_mean_rh", "daily_sum_rain", "daily_sum_leaf_wetness") %in% names(window_pane_demo_data$weather)))
  expect_true(all(c("site_id", "time", "temp", "rh", "rain", "leaf_wetness") %in% names(window_pane_demo_data$weather_hourly)))
  expect_true(all(c("site_id", "assessment_time", "planting_time", "disease_intensity") %in% names(window_pane_demo_data$assessments)))
  expect_equal(length(unique(table(window_pane_demo_data$weather$site_id))), 1)
  expect_gt(length(unique(window_pane_demo_data$assessments$planting_time)), 1)
  expect_false(all(diff(window_pane_demo_data$assessments$planting_time) == 86400))

  days_after_planting <- as.numeric(difftime(
    window_pane_demo_data$assessments$assessment_time,
    window_pane_demo_data$assessments$planting_time,
    units = "days"
  ))
  expect_true(all(days_after_planting >= 85 & days_after_planting <= 95))

  days_before_planting <- mapply(function(site_id, planting_time) {
    site_weather <- window_pane_demo_data$weather[window_pane_demo_data$weather$site_id == site_id, , drop = FALSE]
    as.numeric(difftime(planting_time, min(site_weather$time), units = "days"))
  }, window_pane_demo_data$assessments$site_id, window_pane_demo_data$assessments$planting_time)
  expect_true(all(days_before_planting >= 30))

  expect_true(all(c("weather_daily", "assessments", "variable_specs") %in% names(fda_demo_data)))
  expect_true(all(c("site_id", "dap", "rh", "rain", "leaf_wetness") %in% names(fda_demo_data$weather_daily)))
  expect_true(all(c("site_id", "wm", "wm_class") %in% names(fda_demo_data$assessments)))
})

test_that("simulate_weather_series validates multi-series arguments", {
  expect_error(
    simulate_weather_series(n_series = 0),
    "`n_series` must be a single number greater than or equal to 1."
  )
  expect_error(
    simulate_weather_series(n_series = 2, id_col = ""),
    "`id_col` must be `NULL` or a single non-empty string."
  )
})

test_that("simulate_assessment_data creates one percent assessment per series", {
  weather <- simulate_weather_series(
    days = 35,
    n_series = 4,
    id_col = "site_id",
    seed = 1
  )
  assessments <- simulate_assessment_data(weather, id_col = "site_id", seed = 123)

  expect_equal(nrow(assessments), 4)
  expect_true(all(c("site_id", "assessment_id", "assessment_time", "response_type", "disease_intensity") %in% names(assessments)))
  expect_true(all(assessments$disease_intensity >= 0 & assessments$disease_intensity <= 100))
  expect_equal(unique(assessments$response_type), "percent")
})

test_that("simulate_assessment_data supports binary and ordinal responses", {
  weather <- simulate_weather_series(
    days = 20,
    n_series = 6,
    id_col = "site_id",
    seed = 1
  )

  binary <- simulate_assessment_data(weather, id_col = "site_id", response_type = "binary", seed = 10)
  ordinal <- simulate_assessment_data(
    weather,
    id_col = "site_id",
    response_type = "ordinal",
    n_levels = 10,
    seed = 10
  )

  expect_true(all(binary$disease_intensity %in% c(0, 1)))
  expect_true(all(ordinal$disease_intensity %in% 0:9))
  expect_error(
    simulate_assessment_data(weather, id_col = "missing"),
    "`id_col` 'missing' was not found"
  )
})

test_that("make_functional_matrix reshapes long data into common-grid form", {
  weather <- simulate_weather_series(days = 5, seed = 22)
  weather$subject <- "S1"
  weather$dap <- seq_len(nrow(weather)) - 3

  mat <- make_functional_matrix(
    data = weather,
    id_col = "subject",
    time_col = "dap",
    value_col = "temp"
  )

  expect_equal(nrow(mat$y), 1)
  expect_equal(ncol(mat$y), length(unique(weather$dap)))
  expect_equal(mat$id[[1]], "S1")
  expect_equal(mat$time, sort(unique(weather$dap)))
})

test_that("functional_group_summary returns mean curves and differences", {
  weather <- simulate_weather_series(days = 6, seed = 5)
  weather$subject <- rep(sprintf("S%02d", 1:6), each = 24)
  weather <- weather[seq_len(144), , drop = FALSE]
  weather$group <- rep(c("absent", "present"), each = 72)
  weather$dap <- rep(seq_len(24) - 5, times = 6)

  out <- functional_group_summary(
    data = weather,
    id_col = "subject",
    time_col = "dap",
    value_col = "temp",
    group_col = "group"
  )

  expect_true(all(c("group", "time", "mean_value") %in% names(out$summary)))
  expect_true(all(c("time", "difference") %in% names(out$difference)))
})

test_that("extract_interval_features creates interval summaries", {
  weather <- simulate_weather_series(days = 4, seed = 8)
  weather$subject <- rep(sprintf("S%02d", 1:4), each = 24)
  weather <- weather[seq_len(96), , drop = FALSE]
  weather$dap <- rep(seq_len(24) - 3, times = 4)
  intervals <- data.frame(start = c(0, 5), end = c(3, 8))

  features <- extract_interval_features(
    data = weather,
    id_col = "subject",
    time_col = "dap",
    value_col = "rh",
    intervals = intervals
  )

  expect_true(all(c("subject", "rh_mean_0_3", "rh_mean_5_8") %in% names(features)))
  expect_equal(nrow(features), 4)

  multi_stat <- extract_interval_features(
    data = weather,
    id_col = "subject",
    time_col = "dap",
    value_col = "rh",
    intervals = intervals[1, , drop = FALSE],
    statistics = list(avg = "mean", p90 = function(x, na.rm = TRUE) stats::quantile(x, 0.9, na.rm = na.rm))
  )

  expect_true(all(c("rh_avg_0_3", "rh_p90_0_3") %in% names(multi_stat)))
})



