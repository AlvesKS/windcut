test_that("condition summaries evaluate expressions in data frames", {
  weather <- data.frame(
    temp = c(18, 22, 24, 16, 25),
    rh = c(91, 95, 80, 92, NA),
    rain = c(0, 1, 2, 3, 4),
    leaf_wetness = c(0, 1, 1, 0, 1)
  )

  expect_equal(count_when(temp > 20 & rh > 90)(weather), 1L)
  expect_equal(proportion_when(temp > 20 & rh > 90)(weather), 1 / 4)
  expect_equal(sum_when(rain, temp > 20)(weather), 7)
  expect_equal(mean_when(rain, temp > 20)(weather), 7 / 3)
  expect_equal(max_when(rain, temp > 20)(weather), 4)
  expect_equal(min_when(rain, temp > 20)(weather), 1)
})

test_that("condition spell summaries detect consecutive favorable rows", {
  weather <- data.frame(
    temp = c(17, 20, 22, 23, 15, 21),
    rh = c(92, 91, 93, NA, 90, 95),
    leaf_wetness = c(1, 1, 1, 1, 0, 1)
  )

  expect_equal(max_consecutive_when(temp >= 18 & rh >= 90)(weather), 2L)
  expect_equal(spell_count_when(temp >= 18 & rh >= 90)(weather), 2L)
  expect_equal(mean_spell_duration_when(temp >= 18 & rh >= 90)(weather), 1.5)
  expect_equal(max_spell_duration_when(temp >= 18 & rh >= 90)(weather), 2L)
})

test_that("condition summaries validate expression shape", {
  weather <- data.frame(temp = 1:3, rh = 91:93)

  expect_error(count_when(temp)(weather), "logical value per row")
  expect_error(sum_when(rh > 90, temp > 1)(weather), "numeric value per row")
})
