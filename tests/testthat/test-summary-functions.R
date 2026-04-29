test_that("threshold count summaries return expected integer counts", {
  expect_equal(count_above(2)(c(1, 2, 3)), 1L)
  expect_equal(count_at_or_above(2)(c(1, 2, 3)), 2L)
  expect_equal(count_below(2)(c(1, 2, 3)), 1L)
  expect_equal(count_at_or_below(2)(c(1, 2, 3)), 2L)
  expect_equal(count_between(2, 3)(c(1, 2, 3, 4)), 2L)
  expect_equal(count_between(2, 3, inclusive = FALSE)(c(1, 2, 2.5, 3)), 1L)
})

test_that("threshold proportions use valid observations as denominator", {
  expect_equal(proportion_above(1)(c(1, 2, 3, NA)), 2 / 3)
  expect_equal(proportion_at_or_above(2)(c(1, 2, 3, NA)), 2 / 3)
  expect_equal(proportion_between(2, 3)(c(1, 2, 3, 4, NA)), 2 / 4)
  expect_true(is.na(proportion_above(1)(c(NA_real_, NA_real_))))
  expect_true(is.na(proportion_above(1)(numeric())))
})

test_that("conditional value summaries handle no selected values", {
  expect_true(is.na(mean_above(10)(c(1, 2, 3))))
  expect_equal(sum_above(10)(c(1, 2, 3)), 0)
  expect_equal(sum_between(2, 3)(c(1, 2, 3, 4)), 5)
  expect_equal(mean_between(2, 3)(c(1, 2, 3, 4)), 2.5)
})

test_that("thermal-time summaries are stable", {
  expect_equal(degree_hours_above(10)(c(8, 10, 12, 15)), 7)
  expect_equal(degree_hours_below(10)(c(8, 10, 12, 15)), 2)
  expect_equal(degree_days_above(10)(c(8, 10, 12, 15)), 7 / 24)
  expect_equal(thermal_time_above(10)(c(8, 10, 12, 15)), 7)
  expect_equal(thermal_time_between(18, 26)(c(16, 20, 30)), 10)
})

test_that("weather-specific wrappers use readable defaults", {
  expect_equal(humid_hours()(c(80, 90, 95)), 2L)
  expect_equal(dry_hours()(c(55, 60, 70)), 2L)
  expect_equal(rainy_hours()(c(0, 0.1, 2)), 2L)
  expect_equal(rainy_days()(c(0, 0.1, 2)), 2L)
  expect_equal(wet_hours()(c(0, 1, 2)), 2L)
  expect_equal(proportion_wet()(c(0, 1, 2)), 2 / 3)
})

test_that("rain event summaries use consecutive rainy observations", {
  rain <- c(0, 0.3, 0.4, 0, 1.2, 1.1, 0)

  expect_equal(rain_events(0.2)(rain), 2L)
  expect_equal(rain_event_count(0.2)(rain), 2L)
  expect_equal(max_rain_event(0.2)(rain), 2.3)
  expect_equal(mean_rain_event(0.2)(rain), mean(c(0.7, 2.3)))
  expect_equal(hours_since_last_rain(0.2)(rain), 1)
  expect_equal(days_since_last_rain(0.2)(rain), 1)
})

test_that("summary functions validate inputs", {
  expect_error(count_above(NA_real_), "`threshold` must be a single finite number")
  expect_error(count_between(5, 2), "`lower` must be less than or equal to `upper`")
  expect_error(count_above(1)("x"), "numeric vectors")
})
