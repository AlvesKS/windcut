test_that("functional_group_summary supports smoothing methods", {
  data <- do.call(rbind, lapply(1:6, function(i) {
    group <- if (i <= 3) "wm-" else "wm+"
    time <- -2:2
    values <- if (group == "wm+") time + 2 else time
    data.frame(
      subject = sprintf("S%02d", i),
      dap = time,
      wm = group,
      temp = values + i / 50
    )
  }))

  out <- functional_group_summary(
    data = data,
    id_col = "subject",
    time_col = "dap",
    value_col = "temp",
    group_col = "wm",
    method = "lowess",
    smooth_args = list(f = 0.5)
  )

  expect_equal(out$method, "lowess")
  expect_true(all(c("group", "time", "mean_value") %in% names(out$summary)))
  expect_true(all(c("time", "difference") %in% names(out$difference)))
  expect_true(all(c("group", "time", "mean_value") %in% names(out$raw_summary)))
  expect_true(all(c("time", "difference") %in% names(out$raw_difference)))
})

test_that("functional_group_summary can evaluate summaries on a denser time grid", {
  data <- do.call(rbind, lapply(1:4, function(i) {
    group <- if (i <= 2) "wm-" else "wm+"
    data.frame(
      subject = sprintf("S%02d", i),
      dap = -2:2,
      wm = group,
      rh = 80 + (-2:2) + if (group == "wm+") 2 else 0
    )
  }))

  dense_time <- seq(-2, 2, length.out = 25)
  out <- functional_group_summary(
    data = data,
    id_col = "subject",
    time_col = "dap",
    value_col = "rh",
    group_col = "wm",
    time_grid = dense_time,
    method = "spline",
    smooth_args = list(df = 4)
  )

  expect_equal(sort(unique(out$summary$time)), dense_time)
  expect_equal(sort(unique(out$raw_summary$time)), -2:2)
  expect_equal(nrow(out$difference), length(dense_time))
})

test_that("functional plotting functions return ggplot objects", {
  data <- do.call(rbind, lapply(1:6, function(i) {
    group <- if (i <= 3) "wm-" else "wm+"
    time <- -2:2
    values <- if (group == "wm+") sin(time) + 1 else sin(time)
    data.frame(
      subject = sprintf("S%02d", i),
      dap = time,
      wm = group,
      temp = values
    )
  }))

  summary_out <- functional_group_summary(
    data = data,
    id_col = "subject",
    time_col = "dap",
    value_col = "temp",
    group_col = "wm",
    method = "spline",
    smooth_args = list(df = 4)
  )

  p_means <- plot_functional_means(summary_out)
  p_diff <- plot_functional_difference(summary_out)

  expect_s3_class(p_means, "ggplot")
  expect_s3_class(p_diff, "ggplot")
})

test_that("functional_interval_test returns p-values and collapsed intervals", {
  skip_if_not_installed("fdatest")

  set.seed(1)
  time <- -3:3
  group0 <- matrix(rnorm(7 * 8, mean = 0, sd = 0.2), nrow = 8, ncol = 7)
  group1 <- matrix(rnorm(7 * 8, mean = 0, sd = 0.2), nrow = 8, ncol = 7)
  group1[, 5:7] <- group1[, 5:7] + 2

  out <- functional_interval_test(
    group0 = group0,
    group1 = group1,
    n_permutations = 50,
    alpha = 0.05,
    time = time
  )

  expect_true(is.data.frame(out$corrected_p_values))
  expect_true(all(c("time", "corrected_p_value", "significant") %in% names(out$corrected_p_values)))
  expect_true(is.numeric(out$significant_time))
  expect_true(is.data.frame(out$intervals))
})

test_that("plot_functional_on_scalar returns a ggplot for pffr models", {
  skip_if_not_installed("refund")

  set.seed(3)
  yind <- -4:4
  x <- rep(c(-1, 1), each = 6)
  y <- sapply(yind, function(t) 2 + 0.3 * t + x * exp(-(t - 2)^2 / 6) + rnorm(length(x), sd = 0.1))
  y <- as.matrix(y)

  fit <- functional_on_scalar(
    y = y,
    x = x,
    yind = yind,
    formula = y ~ x,
    bs.yindex = list(bs = "ps", k = 8, m = c(2, 1)),
    bs.int = list(bs = "ps", k = 8, m = c(2, 1))
  )

  p <- plot_functional_on_scalar(fit)
  expect_s3_class(p, "ggplot")
})

test_that("run_fda_analysis handles the full multi-variable workflow", {
  skip_if_not_installed("fdatest")

  data <- do.call(rbind, lapply(1:10, function(i) {
    group <- if (i <= 5) "wm-" else "wm+"
    time <- -3:3
    data.frame(
      site_id = sprintf("S%02d", i),
      dap = time,
      wm_class = group,
      rh = 70 + time + if (group == "wm+") c(0, 0, 0, 1, 2, 3, 3) else 0,
      temp = 20 + sin(time) + i / 100
    )
  }))

  out <- run_fda_analysis(
    data = data,
    id_col = "site_id",
    time_col = "dap",
    group_col = "wm_class",
    value_cols = c("rh", "temp"),
    value_labels = c(rh = "Relative humidity", temp = "Temperature"),
    alpha = c(0.05, 0.01),
    n_permutations = 50,
    smooth_method = "spline",
    smooth_args = list(df = 4),
    n_time_grid = 25
  )

  expect_s3_class(out, "windcut_fda_analysis")
  expect_equal(out$value_cols, c("rh", "temp"))
  expect_true(all(c("variable", "variable_label", "alpha", "start", "end", "status") %in% names(out$interval_summary)))
  expect_equal(sort(unique(out$interval_summary$alpha)), c(0.01, 0.05))
  expect_true("rh" %in% names(out$matrices))
  expect_true("rh" %in% names(out$summaries))
})

test_that("high-level FDA plotting and extraction functions return user-facing outputs", {
  skip_if_not_installed("fdatest")

  data <- do.call(rbind, lapply(1:8, function(i) {
    group <- if (i <= 4) "wm-" else "wm+"
    time <- -2:4
    data.frame(
      site_id = sprintf("S%02d", i),
      dap = time,
      wm_class = group,
      rh = 80 + time + if (group == "wm+") c(0, 0, 1, 2, 3, 4, 4) else 0
    )
  }))

  out <- run_fda_analysis(
    data = data,
    id_col = "site_id",
    time_col = "dap",
    group_col = "wm_class",
    value_cols = "rh",
    alpha = 0.05,
    n_permutations = 50,
    smooth_method = "spline",
    smooth_args = list(df = 4)
  )

  expect_s3_class(plot_fda_means(out, "rh"), "ggplot")
  expect_s3_class(plot_fda_difference(out, "rh"), "ggplot")
  expect_s3_class(plot_fda_p_values(out, "rh"), "ggplot")
  expect_s3_class(plot_fda_intervals(out), "ggplot")

  features <- extract_fda_features(out, alpha = 0.05)
  expect_true("site_id" %in% names(features))
  expect_equal(nrow(features), length(unique(data$site_id)))
})

test_that("extract_fda_features accepts different statistics by variable", {
  data <- do.call(rbind, lapply(1:3, function(i) {
    data.frame(
      site_id = sprintf("S%02d", i),
      dap = 1:5,
      rh = 80 + i + 1:5,
      rain = c(0, 0.2, 0, 1, 0.4) * i
    )
  }))

  analysis <- list(
    data = data,
    id_col = "site_id",
    time_col = "dap",
    value_cols = c("rh", "rain"),
    value_labels = c(rh = "Relative humidity", rain = "Rainfall"),
    alpha = 0.05,
    interval_summary = data.frame(
      variable = c("rh", "rain"),
      variable_label = c("Relative humidity", "Rainfall"),
      alpha = c(0.05, 0.05),
      alpha_label = c("alpha = 0.05", "alpha = 0.05"),
      start = c(2, 2),
      end = c(4, 4),
      status = c("Significant interval", "Significant interval"),
      stringsAsFactors = FALSE
    )
  )
  class(analysis) <- "windcut_fda_analysis"

  features <- extract_fda_features(
    analysis,
    alpha = 0.05,
    statistics = list(
      rh = c("mean", "min"),
      rain = list(total = "sum", max = "max")
    )
  )

  expect_true(all(c(
    "fda_rh_mean_2_4",
    "fda_rh_min_2_4",
    "fda_rain_total_2_4",
    "fda_rain_max_2_4"
  ) %in% names(features)))
  expect_equal(features$fda_rain_total_2_4[features$site_id == "S01"], 1.2)

  subset_features <- extract_fda_features(
    analysis,
    alpha = 0.05,
    value_cols = "rh",
    statistics = list(
      rh = "mean",
      rain = "sum"
    )
  )

  expect_true("fda_rh_mean_2_4" %in% names(subset_features))
  expect_false("fda_rain_sum_2_4" %in% names(subset_features))
})

test_that("fit_fda_group_model fits pffr models from FDA workflow objects", {
  skip_if_not_installed("fdatest")
  skip_if_not_installed("refund")

  set.seed(4)
  data <- do.call(rbind, lapply(1:10, function(i) {
    group <- if (i <= 5) "wm-" else "wm+"
    time <- -4:4
    data.frame(
      site_id = sprintf("S%02d", i),
      dap = time,
      wm_class = group,
      rh = 75 + 0.5 * time + if (group == "wm+") exp(-(time - 2)^2 / 5) else 0 + rnorm(length(time), sd = 0.05)
    )
  }))

  out <- run_fda_analysis(
    data = data,
    id_col = "site_id",
    time_col = "dap",
    group_col = "wm_class",
    value_cols = "rh",
    n_permutations = 50,
    smooth_method = "spline",
    smooth_args = list(df = 4)
  )

  fit <- fit_fda_group_model(
    out,
    value_col = "rh",
    bs.yindex = list(bs = "ps", k = 8, m = c(2, 1)),
    bs.int = list(bs = "ps", k = 8, m = c(2, 1))
  )

  expect_s3_class(fit, "pffr")
})



