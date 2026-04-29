test_that("window summaries accept strings, functions, and conditions", {
  weather <- data.frame(
    time = as.POSIXct("2024-01-01 00:00:00", tz = "UTC") + seq(0, by = 3600, length.out = 6),
    temp = c(17, 18, 22, 26, 30, NA),
    rh = c(85, 91, 93, 88, 95, 90),
    rain = c(0, 0.3, 0.4, 0, 1, 0),
    leaf_wetness = c(0, 1, 1, 0, 1, 1)
  )

  out <- summarise_weather_window(
    weather,
    statistics = list(
      temp = list(mean = "mean", hours_18_26 = count_between(18, 26)),
      rh = list(humid_hours = humid_hours(90)),
      rain = list(total = "sum", events = rain_events(0.2)),
      .conditions = list(
        favorable_hours = count_when(temp >= 18 & temp <= 26 & rh >= 90),
        warm_rain_total = sum_when(rain, temp >= 18 & temp <= 28)
      )
    )
  )

  expect_true(all(c(
    "temp_mean",
    "temp_hours_18_26",
    "rh_humid_hours",
    "rain_total",
    "rain_events",
    "favorable_hours",
    "warm_rain_total"
  ) %in% names(out)))
  expect_equal(out$temp_hours_18_26, 3)
  expect_equal(out$favorable_hours, 2)
  expect_equal(out$warm_rain_total, 0.7)
})

test_that("empty windows let summary functions define sensible defaults", {
  empty_weather <- data.frame(
    time = as.POSIXct(character(), tz = "UTC"),
    temp = numeric(),
    rh = numeric(),
    rain = numeric(),
    leaf_wetness = numeric()
  )

  out <- summarise_weather_window(
    empty_weather,
    statistics = list(
      temp = list(mean = "mean", hot_count = count_above(30)),
      rain = list(total = "sum", events = rain_events(0.2)),
      .conditions = list(
        favorable_hours = count_when(temp >= 18 & rh >= 90)
      )
    )
  )

  expect_equal(out$n_obs, 0)
  expect_true(is.na(out$temp_mean))
  expect_equal(out$temp_hot_count, 0)
  expect_equal(out$rain_total, 0)
  expect_equal(out$rain_events, 0)
  expect_equal(out$favorable_hours, 0)
})

test_that("window_pane keeps old summary_statistics behavior and supports conditions", {
  weather <- simulate_weather_series(days = 15, seed = 1)
  weather <- weather |>
    derive_vpd(temp, rh) |>
    derive_leaf_wetness_from_rh(rh, threshold = 90)
  windows <- make_windows(min_offset = -4, max_offset = 0, width = 2)
  assessments <- data.frame(
    assessment_id = "A1",
    assessment_time = as.POSIXct("2024-01-10 00:00:00", tz = "UTC")
  )

  old_features <- window_pane(
    weather = weather,
    assessments = assessments,
    windows = windows,
    id_col = "assessment_id",
    statistics = list(temp = c("mean", "max"), rain = "sum")
  )

  new_features <- window_pane(
    weather = weather,
    assessments = assessments,
    windows = windows,
    id_col = "assessment_id",
    statistics = list(
      temp = list(mean = "mean", hours_18_26 = count_between(18, 26)),
      rh = list(humid_hours = humid_hours(90)),
      rain = list(total = "sum"),
      leaf_wetness_est = list(wet_hours = wet_hours(0)),
      .conditions = list(
        favorable_hours = count_when(temp >= 18 & temp <= 26 & rh >= 90),
        max_favorable_spell = max_consecutive_when(temp >= 18 & temp <= 26 & rh >= 90)
      )
    )
  )

  expect_true(any(grepl("^temp_mean_window_", names(old_features))))
  expect_true(any(grepl("^temp_hours_18_26_window_", names(new_features))))
  expect_true(any(grepl("^favorable_hours_window_", names(new_features))))
  expect_true(any(grepl("^max_favorable_spell_window_", names(new_features))))
  expect_true(all(vapply(new_features[grep("favorable|wet_hours|humid_hours", names(new_features))], is.numeric, logical(1))))
})

test_that("condition-only statistics do not require default weather column names", {
  weather <- data.frame(
    site_id = "S01",
    time = as.POSIXct("2024-01-01 00:00:00", tz = "UTC") + seq(0, by = 3600, length.out = 4),
    temp2m = c(17, 20, 24, 28),
    relhum = c(88, 92, 95, 80)
  )
  windows <- make_windows(min_offset = -4, max_offset = 0, width = 4)
  assessments <- data.frame(
    site_id = "S01",
    assessment_time = as.POSIXct("2024-01-01 04:00:00", tz = "UTC")
  )

  features <- window_pane(
    weather = weather,
    assessments = assessments,
    windows = windows,
    id_col = "site_id",
    statistics = list(
      .conditions = list(
        favorable_hours = count_when(temp2m >= 18 & temp2m <= 26 & relhum >= 90)
      )
    ),
    unit = "hours"
  )

  expect_equal(features$favorable_hours_window_m04_z00, 2)
})

test_that("aggregate_weather_daily supports condition summaries", {
  weather <- data.frame(
    site_id = "S01",
    time = as.POSIXct("2024-01-01 00:00:00", tz = "UTC") + seq(0, by = 3600, length.out = 4),
    temp2m = c(17, 20, 24, 28),
    relhum = c(88, 92, 95, 80)
  )

  daily <- aggregate_weather_daily(
    weather,
    id_col = "site_id",
    statistics = list(
      .conditions = list(
        favorable_hours = count_when(temp2m >= 18 & temp2m <= 26 & relhum >= 90)
      )
    )
  )

  expect_equal(daily$daily_favorable_hours, 2)
})
