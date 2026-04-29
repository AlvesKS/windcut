test_that("spell summaries detect consecutive runs", {
  x <- c(1, 3, 4, 1, 5, 6, 7)

  expect_equal(max_consecutive_above(2)(x), 3L)
  expect_equal(spell_count_above(2)(x), 2L)
  expect_equal(mean_spell_duration_above(2)(x), 2.5)
  expect_equal(max_spell_duration_above(2)(x), 3L)
})

test_that("NA values break spells when na.rm is TRUE", {
  x <- c(1, 3, 4, NA, 5, 6, 7)

  expect_equal(max_consecutive_above(2)(x), 3L)
  expect_equal(spell_count_above(2)(x), 2L)
  expect_true(is.na(max_consecutive_above(2, na.rm = FALSE)(x)))
})

test_that("between spell summaries work with inclusive boundaries", {
  x <- c(17, 18, 20, 27, 22, 25)

  expect_equal(max_consecutive_between(18, 26)(x), 2L)
  expect_equal(spell_count_between(18, 26)(x), 2L)
})

test_that("spell summaries handle empty vectors and no runs", {
  expect_equal(max_consecutive_above(2)(numeric()), 0L)
  expect_equal(spell_count_above(2)(numeric()), 0L)
  expect_true(is.na(mean_spell_duration_above(2)(numeric())))
  expect_equal(max_consecutive_above(10)(c(1, 2, 3)), 0L)
})
