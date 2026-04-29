test_that("weather derivative functions add named columns", {
  weather <- data.frame(
    temp = c(20, 24, 28),
    rh = c(85, 95, 90),
    tmax = c(25, 30, 32),
    tmin = c(15, 18, 20),
    rain = c(0, 1, 0)
  )

  out <- weather |>
    derive_dew_point(temp, rh) |>
    derive_vpd(temp, rh) |>
    derive_temperature_range(tmax, tmin) |>
    derive_leaf_wetness_from_rh(rh, threshold = 90) |>
    derive_leaf_wetness_from_rh_temp(rh, temp, rh_threshold = 90, temp_range = c(20, 26), name = "wet_warm") |>
    derive_favorable_condition(temp >= 18 & temp <= 26 & rh >= 90)

  expect_true(all(c("dew_point", "vpd", "temp_range", "leaf_wetness_est", "wet_warm", "favorable") %in% names(out)))
  expect_equal(names(weather), c("temp", "rh", "tmax", "tmin", "rain"))
  expect_equal(out$leaf_wetness_est, c(0L, 1L, 1L))
  expect_equal(out$favorable, c(0L, 1L, 0L))
})

test_that("weather derivative functions validate RH and names", {
  weather <- data.frame(temp = c(20, 24), rh = c(85, 101))

  expect_error(derive_vpd(weather, temp, rh), "0 to 100")
  expect_error(derive_dew_point(weather, missing, rh), "Missing required columns")
  expect_error(derive_leaf_wetness_from_rh(weather, rh, name = ""), "`name`")
})

test_that("weather derivative functions validate numeric weather columns", {
  weather <- data.frame(temp = c("20", "24"), rh = c(85, 90))

  expect_error(derive_vpd(weather, temp, rh), "Column `temp` must be numeric")
  expect_error(derive_dew_point(weather, temp, rh), "Column `temp` must be numeric")
})

test_that("dew point returns NA when relative humidity is zero", {
  weather <- data.frame(temp = c(20, 24), rh = c(0, 90))

  out <- derive_dew_point(weather, temp, rh)

  expect_true(is.na(out$dew_point[[1]]))
  expect_true(is.finite(out$dew_point[[2]]))
})
