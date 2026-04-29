test_that("make_windows creates fixed-width sliding windows", {
  windows <- make_windows(min_offset = 1, max_offset = 7, width = 5)

  expect_equal(nrow(windows), 2)
  expect_true(all(c("relative_start", "relative_end", "width", "label") %in% names(windows)))
  expect_equal(windows$label, c("window_p01_p06", "window_p02_p07"))
  expect_equal(windows$width, c(5, 5))
})

test_that("make_windows can slide by more than one relative-time unit", {
  windows <- make_windows(min_offset = 1, max_offset = 11, width = 5, slide_by = 2)

  expect_equal(windows$label, c("window_p01_p06", "window_p03_p08", "window_p05_p10"))
  expect_equal(windows$relative_start, c(1, 3, 5))
})

test_that("make_windows can store reference column metadata", {
  windows <- make_windows(min_offset = 1, max_offset = 7, width = 5, reference_col = "planting_time")

  expect_equal(attr(windows, "reference_col"), "planting_time")
  expect_error(make_windows(reference_col = c("a", "b")), "`reference_col` must be a single character string")
})

test_that("make_windows creates variable-width windows", {
  windows <- make_windows(
    min_offset = 1,
    max_offset = 5,
    width = 2:3
  )

  expect_equal(
    windows$label,
    c("window_p01_p03", "window_p01_p04", "window_p02_p04", "window_p02_p05", "window_p03_p05")
  )
  expect_equal(windows$width, c(2, 3, 2, 3, 2))
})

test_that("make_windows accepts several durations and slide_by", {
  windows <- make_windows(
    min_offset = 1,
    max_offset = 9,
    width = c(2, 4, 6),
    slide_by = 2
  )

  expect_equal(
    windows$label,
    c("window_p01_p03", "window_p01_p05", "window_p01_p07", "window_p03_p05", "window_p03_p07", "window_p03_p09", "window_p05_p07", "window_p05_p09", "window_p07_p09")
  )
  expect_equal(windows$width, c(2, 4, 6, 2, 4, 6, 2, 4, 2))
})

test_that("make_windows can generate regular variable-width sequences", {
  windows <- make_windows(
    min_offset = 1,
    max_offset = 9,
    width = seq(2, 4, by = 2),
    slide_by = 2
  )

  expect_equal(
    windows$label,
    c("window_p01_p03", "window_p01_p05", "window_p03_p05", "window_p03_p07", "window_p05_p07", "window_p05_p09", "window_p07_p09")
  )
  expect_equal(windows$width, c(2, 4, 2, 4, 2, 4, 2))
})

test_that("make_windows treats multi-value width as variable mode", {
  windows <- make_windows(min_offset = 1, max_offset = 4, width = c(2, 3))

  expect_equal(windows$label, c("window_p01_p03", "window_p01_p04", "window_p02_p04"))
})

test_that("make_windows supports windows before and across the reference", {
  before <- make_windows(min_offset = -5, max_offset = -1, width = 3)
  around <- make_windows(min_offset = -2, max_offset = 2, width = 4)

  expect_equal(before$label, c("window_m05_m02", "window_m04_m01"))
  expect_equal(around$label, "window_m02_p02")
})

test_that("make_windows requires positive durations and slide_by", {
  expect_error(make_windows(width = 0), "`width` must contain values greater than zero")
  expect_error(make_windows(slide_by = 0), "`slide_by` must be greater than zero")
  expect_error(make_windows(width = c(2, 0)), "`width` must contain values greater than zero")
  expect_error(make_windows(width = c(2, 2.5)), "`width` must contain whole-number values")
})

test_that("plot_window_pane returns ggplot objects", {
  windows <- make_windows(min_offset = -7, max_offset = 14, width = 7, slide_by = 7)
  variable_windows <- make_windows(min_offset = -7, max_offset = 7, width = c(3, 5), slide_by = 3)
  many_widths <- make_windows(min_offset = -10, max_offset = 10, width = 1:8, slide_by = 4)

  expect_s3_class(plot_window_pane(windows), "ggplot")
  expect_s3_class(plot_window_pane(variable_windows, color_by = "width"), "ggplot")
  expect_s3_class(plot_window_pane(many_widths, color_by = "width"), "ggplot")
  expect_s3_class(plot_window_pane(windows, color_by = "none", max_windows = Inf), "ggplot")
})

test_that("plot_window_pane validates input", {
  expect_error(plot_window_pane(data.frame()), "`windows` must contain")
  expect_error(plot_window_pane(make_windows(), max_windows = 0), "`max_windows` must be a positive number")
  expect_error(plot_window_pane(make_windows(min_offset = 1, max_offset = 1, width = 5)), "`windows` must contain at least one row")
})

test_that("scan_windows returns one row per candidate window", {
  weather <- example_weather_data(days = 10)
  windows <- make_windows(min_offset = 1, max_offset = 4, width = 2)

  scanned <- scan_windows(
    weather = weather,
    reference_time = as.POSIXct("2024-01-10 00:00:00", tz = "UTC"),
    windows = windows,
    unit = "days"
  )

  expect_equal(nrow(scanned), nrow(windows))
  expect_true(all(c("temp_mean", "rain_sum", "rh_mean") %in% names(scanned)))
})

test_that("scan_windows accepts a general reference time", {
  weather <- example_weather_data(days = 10)
  windows <- make_windows(min_offset = -4, max_offset = -1, width = 2)

  scanned <- scan_windows(
    weather = weather,
    reference_time = as.POSIXct("2024-01-10 00:00:00", tz = "UTC"),
    windows = windows,
    unit = "days"
  )

  expect_equal(nrow(scanned), nrow(windows))
  expect_true(all(scanned$window_end <= as.POSIXct("2024-01-10 00:00:00", tz = "UTC")))
})

test_that("scan_windows accepts user-selected statistics", {
  weather <- example_weather_data(days = 10)
  windows <- make_windows(min_offset = 1, max_offset = 4, width = 2)

  scanned <- scan_windows(
    weather = weather,
    reference_time = as.POSIXct("2024-01-10 00:00:00", tz = "UTC"),
    windows = windows,
    statistics = c("mean", "median", "max")
  )

  expect_true(all(c("temp_median", "rain_max", "leaf_wetness_median") %in% names(scanned)))
  expect_false("rain_sum" %in% names(scanned))
})

test_that("scan_windows accepts arbitrary weather columns and custom statistics", {
  weather <- example_weather_data(days = 10)
  names(weather)[names(weather) == "temp"] <- "temp2m"
  names(weather)[names(weather) == "rain"] <- "prectot"
  weather$sradiation <- seq_len(nrow(weather))
  windows <- make_windows(min_offset = 1, max_offset = 4, width = 2)

  scanned <- scan_windows(
    weather = weather,
    reference_time = as.POSIXct("2024-01-10 00:00:00", tz = "UTC"),
    windows = windows,
    weather_cols = c("temp2m", "prectot", "sradiation"),
    statistics = list(
      avg = "mean",
      spread = stats::sd,
      p90 = function(x, na.rm = TRUE) stats::quantile(x, probs = 0.9, na.rm = na.rm)
    )
  )

  expect_true(all(c("temp2m_avg", "prectot_spread", "sradiation_p90") %in% names(scanned)))
  expect_false("rh_mean" %in% names(scanned))
})

test_that("scan_windows accepts custom weather column names", {
  weather <- example_weather_data(days = 10)
  names(weather)[names(weather) == "time"] <- "timestamp"
  names(weather)[names(weather) == "temp"] <- "air_temp"
  names(weather)[names(weather) == "rh"] <- "rel_humidity"
  names(weather)[names(weather) == "rain"] <- "rainfall"
  names(weather)[names(weather) == "leaf_wetness"] <- "wet_hours"

  windows <- make_windows(min_offset = 1, max_offset = 4, width = 2)
  scanned <- scan_windows(
    weather = weather,
    reference_time = as.POSIXct("2024-01-10 00:00:00", tz = "UTC"),
    windows = windows,
    time_col = "timestamp",
    temp_col = "air_temp",
    rh_col = "rel_humidity",
    rain_col = "rainfall",
    leaf_wetness_col = "wet_hours"
  )

  expect_equal(nrow(scanned), nrow(windows))
  expect_true(all(c("temp_mean", "rain_sum", "rh_mean", "leaf_wetness_sum") %in% names(scanned)))
})

test_that("window_pane builds a model-ready wide table", {
  weather <- example_weather_data(days = 15)
  windows <- make_windows(min_offset = 1, max_offset = 4, width = 2)
  assessments <- data.frame(
    assessment_id = c("A1", "A2"),
    assessment_time = as.POSIXct(c("2024-01-10 00:00:00", "2024-01-12 00:00:00"), tz = "UTC"),
    disease_intensity = c(15.2, 19.1)
  )

  features <- window_pane(
    weather = weather,
    assessments = assessments,
    windows = windows,
    reference_col = "assessment_time",
    id_col = "assessment_id",
    response_col = "disease_intensity",
    unit = "days"
  )

  expect_equal(nrow(features), 2)
  expect_true("assessment_id" %in% names(features))
  expect_true("disease_intensity" %in% names(features))
  expect_true(any(grepl("^temp_mean_window_p01_p03$", names(features))))
})

test_that("window_pane uses selected statistics in feature names", {
  weather <- example_weather_data(days = 10)
  windows <- make_windows(min_offset = 1, max_offset = 4, width = 2)
  assessments <- data.frame(
    assessment_id = "A1",
    assessment_time = as.POSIXct("2024-01-10 00:00:00", tz = "UTC")
  )

  features <- window_pane(
    weather = weather,
    assessments = assessments,
    windows = windows,
    reference_col = "assessment_time",
    id_col = "assessment_id",
    statistics = c("median", "sum")
  )

  expect_true("temp_median_window_p01_p03" %in% names(features))
  expect_true("rain_sum_window_p01_p03" %in% names(features))
  expect_false("temp_mean_window_p01_p03" %in% names(features))
})

test_that("window_pane supports named arbitrary weather columns", {
  weather <- example_weather_data(days = 10)
  names(weather)[names(weather) == "temp"] <- "temp2m"
  weather$sradiation <- seq_len(nrow(weather))
  windows <- make_windows(min_offset = 1, max_offset = 4, width = 2)
  assessments <- data.frame(
    assessment_id = "A1",
    assessment_time = as.POSIXct("2024-01-10 00:00:00", tz = "UTC")
  )

  features <- window_pane(
    weather = weather,
    assessments = assessments,
    windows = windows,
    reference_col = "assessment_time",
    id_col = "assessment_id",
    weather_cols = c(air_temp = "temp2m", solar = "sradiation"),
    statistics = list(mean = mean, sd = stats::sd)
  )

  expect_true("air_temp_mean_window_p01_p03" %in% names(features))
  expect_true("solar_sd_window_p01_p03" %in% names(features))
})

test_that("statistics can be selected by arbitrary weather variable", {
  weather <- example_weather_data(days = 10)
  names(weather)[names(weather) == "temp"] <- "temp2m"
  weather$sradiation <- seq_len(nrow(weather))
  windows <- make_windows(min_offset = 1, max_offset = 4, width = 2)

  scanned <- scan_windows(
    weather = weather,
    reference_time = as.POSIXct("2024-01-10 00:00:00", tz = "UTC"),
    windows = windows,
    weather_cols = c("temp2m", "sradiation"),
    statistics = list(
      temp2m = c("mean", "max"),
      sradiation = list(total = "sum")
    )
  )

  expect_true(all(c("temp2m_mean", "temp2m_max", "sradiation_total") %in% names(scanned)))
  expect_false("sradiation_mean" %in% names(scanned))
})

test_that("threshold counts are regular statistics", {
  weather <- example_weather_data(days = 10)
  windows <- make_windows(min_offset = 1, max_offset = 4, width = 2)

  scanned <- scan_windows(
    weather = weather,
    reference_time = as.POSIXct("2024-01-10 00:00:00", tz = "UTC"),
    windows = windows,
    statistics = list(
      rh = list(hours_at_or_above_90 = count_at_or_above(90))
    )
  )

  expect_true("rh_hours_at_or_above_90" %in% names(scanned))
  expect_true(all(scanned$rh_hours_at_or_above_90 >= 0, na.rm = TRUE))
  expect_error(count_at_or_above(NA_real_), "`threshold` must be a single finite number")
})

test_that("window_pane can use site-specific reference dates", {
  weather <- simulate_weather_series(
    days = 30,
    n_series = 3,
    id_col = "site_id",
    seed = 1
  )
  windows <- make_windows(min_offset = 1, max_offset = 5, width = 3)
  references <- data.frame(
    site_id = sprintf("S%02d", 1:3),
    planting_time = as.POSIXct(
      c("2024-01-20 00:00:00", "2024-01-22 00:00:00", "2024-01-24 00:00:00"),
      tz = "UTC"
    ),
    disease_intensity = c(10, 30, 50)
  )

  features <- window_pane(
    weather = weather,
    assessments = references,
    windows = windows,
    reference_col = "planting_time",
    id_col = "site_id",
    response_col = "disease_intensity",
    unit = "days"
  )

  expect_equal(nrow(features), 3)
  expect_true("planting_time" %in% names(features))
  expect_equal(features$planting_time, references$planting_time)
  expect_true(any(grepl("^temp_mean_window_p01_p04$", names(features))))
})

test_that("window_pane accepts custom weather column names", {
  weather <- simulate_weather_series(
    days = 30,
    n_series = 3,
    id_col = "site_id",
    seed = 1
  )
  names(weather)[names(weather) == "time"] <- "timestamp"
  names(weather)[names(weather) == "temp"] <- "air_temp"
  names(weather)[names(weather) == "rh"] <- "rel_humidity"
  names(weather)[names(weather) == "rain"] <- "rainfall"
  names(weather)[names(weather) == "leaf_wetness"] <- "wet_hours"

  references <- data.frame(
    site_id = sprintf("S%02d", 1:3),
    assessment_time = as.POSIXct(
      c("2024-01-20 00:00:00", "2024-01-22 00:00:00", "2024-01-24 00:00:00"),
      tz = "UTC"
    ),
    disease_intensity = c(10, 30, 50)
  )
  windows <- make_windows(min_offset = 1, max_offset = 5, width = 3)

  features <- window_pane(
    weather = weather,
    assessments = references,
    windows = windows,
    reference_col = "assessment_time",
    id_col = "site_id",
    response_col = "disease_intensity",
    time_col = "timestamp",
    temp_col = "air_temp",
    rh_col = "rel_humidity",
    rain_col = "rainfall",
    leaf_wetness_col = "wet_hours"
  )

  expect_equal(nrow(features), 3)
  expect_true(any(grepl("^temp_mean_window_p01_p04$", names(features))))
  expect_true(any(grepl("^rh_mean_window_p01_p04$", names(features))))
})

test_that("window_pane can infer reference_col from windows metadata", {
  weather <- simulate_weather_series(
    days = 30,
    n_series = 3,
    id_col = "site_id",
    seed = 1
  )
  windows <- make_windows(min_offset = 1, max_offset = 5, width = 3, reference_col = "planting_time")
  references <- data.frame(
    site_id = sprintf("S%02d", 1:3),
    planting_time = as.POSIXct(
      c("2024-01-20 00:00:00", "2024-01-22 00:00:00", "2024-01-24 00:00:00"),
      tz = "UTC"
    )
  )

  features <- window_pane(
    weather = weather,
    assessments = references,
    windows = windows,
    id_col = "site_id",
    unit = "days"
  )

  expect_true("planting_time" %in% names(features))
  expect_true(any(grepl("^temp_mean_window_p01_p04$", names(features))))
})

test_that("window_pane validates id and response columns clearly", {
  weather <- example_weather_data(days = 10)
  windows <- make_windows(min_offset = 1, max_offset = 4, width = 2)
  assessments <- data.frame(
    assessment_time = as.POSIXct("2024-01-10 00:00:00", tz = "UTC"),
    disease_intensity = 20
  )

  expect_error(
    window_pane(
      weather = weather,
      assessments = assessments,
      windows = windows,
      reference_col = "assessment_time",
      id_col = "site_id"
    ),
    "`id_col` 'site_id' was not found in `assessments`."
  )

  expect_true(is.data.frame(
    window_pane(
      weather = weather,
      assessments = data.frame(
        assessment_id = "A1",
        assessment_time = as.POSIXct("2024-01-10 00:00:00", tz = "UTC")
      ),
      windows = windows,
      reference_col = "assessment_time",
      id_col = "assessment_id"
    )
  ))

  expect_error(
    window_pane(
      weather = weather,
      assessments = assessments,
      windows = windows,
      reference_col = "assessment_time",
      response_col = "missing_response"
    ),
    "`response_col` 'missing_response' was not found in `assessments`."
  )
})

test_that("screen_window_features ranks window features by association", {
  weather <- simulate_weather_series(
    days = 45,
    n_series = 8,
    id_col = "site_id",
    seed = 100
  )
  assessments <- simulate_assessment_data(weather, id_col = "site_id", seed = 101)
  windows <- make_windows(min_offset = 1, max_offset = 6, width = 2)

  features <- window_pane(
    weather = weather,
    assessments = assessments,
    windows = windows,
    reference_col = "assessment_time",
    id_col = "site_id",
    response_col = "disease_intensity"
  )

  screened <- screen_window_features(features, response_col = "disease_intensity")

  expect_true(nrow(screened) > 0)
  expect_true(all(c("feature", "metric", "window", "estimate", "p_value", "p_adjusted") %in% names(screened)))
  expect_true(any(grepl("^window", screened$window)))
})

test_that("screen_feature_correlations suggests less redundant features", {
  data <- data.frame(
    response = c(1, 2, 3, 4, 5, 6),
    x1 = c(1, 2, 3, 4, 5, 6),
    x2 = c(2, 4, 6, 8, 10, 12),
    x3 = c(6, 5, 4, 3, 2, 1),
    x4 = c(1, 1, 2, 2, 3, 3)
  )

  out <- screen_feature_correlations(
    data,
    exclude_cols = "response",
    method = "pearson",
    threshold = 0.95
  )

  expect_true(is.matrix(out$correlation_matrix))
  expect_true(nrow(out$high_correlations) > 0)
  expect_true(length(out$suggested_features) < 4)
  expect_true(all(out$suggested_features %in% c("x1", "x2", "x3", "x4")))
  expect_true(all(c("feature", "mean_abs_correlation", "suggested", "reason") %in% names(out$feature_summary)))
})

test_that("screen_feature_correlations validates inputs", {
  expect_error(
    screen_feature_correlations(data.frame(x = 1:3), threshold = 1.2),
    "`threshold` must be a single number between 0 and 1"
  )
  expect_error(
    screen_feature_correlations(data.frame(x = 1:3, y = letters[1:3]), feature_cols = c("x", "y")),
    "Feature columns must be numeric"
  )
  expect_error(
    screen_feature_correlations(data.frame(x = 1:3), feature_cols = "missing"),
    "Missing feature columns"
  )
})



