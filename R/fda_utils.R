#' Create a Functional Matrix from Long Data
#'
#' Converts long-format repeated measurements into a common-grid matrix suitable
#' for functional analyses. Each row corresponds to one subject and each column
#' to one time value, such as days after planting.
#'
#' @param data A data frame in long format.
#' @param id_col Subject identifier column.
#' @param time_col Time column, such as relative day or days after planting.
#' @param value_col Measured series column.
#' @param time_grid Optional vector defining the common time grid. If `NULL`,
#'   the sorted unique values in `time_col` are used.
#' @param fill Value used for missing combinations.
#'
#' @return A list with `y`, `time`, and `id`.
#' @export
make_functional_matrix <- function(
    data,
    id_col,
    time_col,
    value_col,
    time_grid = NULL,
    fill = NA_real_
) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }

  required <- c(id_col, time_col, value_col)
  missing_cols <- setdiff(required, names(data))
  if (length(missing_cols) > 0) {
    stop(sprintf("Missing required columns: %s", paste(missing_cols, collapse = ", ")), call. = FALSE)
  }

  if (is.null(time_grid)) {
    time_grid <- sort(unique(data[[time_col]]))
  }

  ids <- unique(data[[id_col]])
  mat <- matrix(fill, nrow = length(ids), ncol = length(time_grid))
  rownames(mat) <- as.character(ids)
  colnames(mat) <- as.character(time_grid)

  row_index <- match(data[[id_col]], ids)
  col_index <- match(data[[time_col]], time_grid)
  mat[cbind(row_index, col_index)] <- data[[value_col]]

  list(
    y = mat,
    time = time_grid,
    id = ids
  )
}

#' Summarise Functional Curves by Group
#'
#' Computes mean functional curves by group and, when exactly two groups are
#' present, also returns the pointwise difference between those curves.
#'
#' @param data A data frame in long format.
#' @param id_col Subject identifier column.
#' @param time_col Time column, such as relative day or days after planting.
#' @param value_col Measured series column.
#' @param group_col Grouping column, such as disease status.
#' @param time_grid Optional grid where the summary curves are evaluated. If
#'   `NULL`, the sorted observed time values are used. Supplying a denser
#'   grid is useful when plotting smoothed curves as continuous functions.
#' @param method Smoothing method used on the group mean curves. One of
#'   `"none"`, `"lowess"`, or `"spline"`.
#' @param smooth_args Named list of extra arguments passed to the smoothing
#'   method. For `"lowess"`, a common choice is `list(f = 0.2)`. For
#'   `"spline"`, typical choices include `list(df = 12)` or `list(spar = 0.6)`.
#'
#' @return A list with grouped summary tables and, when two groups are present,
#'   pointwise difference tables. The returned object includes `summary`,
#'   `difference`, `raw_summary`, `raw_difference`, `method`, and
#'   `smooth_args`.
#' @export
functional_group_summary <- function(
    data,
    id_col,
    time_col,
    value_col,
    group_col,
    time_grid = NULL,
    method = c("none", "lowess", "spline"),
    smooth_args = list()
) {
  method <- match.arg(method)

  required <- c(id_col, time_col, value_col, group_col)
  missing_cols <- setdiff(required, names(data))
  if (length(missing_cols) > 0) {
    stop(sprintf("Missing required columns: %s", paste(missing_cols, collapse = ", ")), call. = FALSE)
  }

  if (is.null(time_grid)) {
    time_grid <- sort(unique(data[[time_col]]))
  }

  group_values <- data[[group_col]]
  groups <- if (is.factor(group_values)) {
    levels(group_values)[levels(group_values) %in% as.character(unique(group_values))]
  } else {
    unique(group_values)
  }

  raw_time_grid <- sort(unique(data[[time_col]]))

  raw_summary_rows <- lapply(groups, function(group) {
    subset <- data[data[[group_col]] == group, , drop = FALSE]
    mean_values <- vapply(raw_time_grid, function(time_value) {
      idx <- subset[[time_col]] == time_value
      mean(subset[[value_col]][idx], na.rm = TRUE)
    }, numeric(1))

    data.frame(
      group = group,
      time = raw_time_grid,
      mean_value = mean_values,
      stringsAsFactors = FALSE
    )
  })

  raw_summary_table <- do.call(rbind, raw_summary_rows)
  summary_rows <- lapply(groups, function(group) {
    group_summary <- raw_summary_table[raw_summary_table$group == group, , drop = FALSE]
    smoothed_values <- .smooth_functional_values(
      time = group_summary$time,
      values = group_summary$mean_value,
      xout = time_grid,
      method = method,
      smooth_args = smooth_args
    )

    data.frame(
      group = group,
      time = time_grid,
      mean_value = smoothed_values,
      stringsAsFactors = FALSE
    )
  })

  summary_table <- do.call(rbind, summary_rows)
  raw_difference_table <- NULL
  difference_table <- NULL

  if (length(groups) == 2) {
    g1_raw <- raw_summary_table[raw_summary_table$group == groups[[1]], c("time", "mean_value")]
    g2_raw <- raw_summary_table[raw_summary_table$group == groups[[2]], c("time", "mean_value")]
    raw_difference_table <- data.frame(
      time = g2_raw$time,
      group_reference = groups[[1]],
      group_comparison = groups[[2]],
      difference = g2_raw$mean_value - g1_raw$mean_value,
      stringsAsFactors = FALSE
    )

    g1 <- summary_table[summary_table$group == groups[[1]], c("time", "mean_value")]
    g2 <- summary_table[summary_table$group == groups[[2]], c("time", "mean_value")]
    difference_table <- data.frame(
      time = g2$time,
      group_reference = groups[[1]],
      group_comparison = groups[[2]],
      difference = g2$mean_value - g1$mean_value,
      stringsAsFactors = FALSE
    )
  }

  list(
    summary = summary_table,
    difference = difference_table,
    raw_summary = raw_summary_table,
    raw_difference = raw_difference_table,
    method = method,
    smooth_args = smooth_args
  )
}

#' Run a Functional Data Analysis Workflow
#'
#' Runs the main FDA workflow for one or more weather variables: creates
#' subject-by-time matrices, computes smoothed group means, compares two groups
#' with interval-wise permutation tests, and stores tidy summaries for plotting
#' and feature extraction.
#'
#' @param data A data frame in long format.
#' @param id_col Subject identifier column.
#' @param time_col Time column, such as relative day or days after planting.
#' @param group_col Grouping column with exactly two groups.
#' @param value_cols Weather variable columns to analyze.
#' @param value_labels Optional named character vector or data frame with
#'   `variable` and `label` columns used for plot labels.
#' @param alpha One or more significance levels used to extract intervals.
#' @param n_permutations Number of permutations used by the interval-wise test.
#' @param smooth_method Smoothing method used for group mean curves. One of
#'   `"none"`, `"lowess"`, or `"spline"`.
#' @param smooth_args Named list of extra arguments passed to the smoothing
#'   method.
#' @param time_grid Optional grid for plotting smoothed group means. If `NULL`,
#'   a dense grid is created from the observed time range.
#' @param n_time_grid Number of points in the dense plotting grid when
#'   `time_grid = NULL`.
#' @param reference_group Optional reference group. If `NULL`, the first group
#'   level or first observed group is used.
#' @param comparison_group Optional comparison group. If `NULL`, the other group
#'   is used.
#' @param quiet Logical; if `TRUE`, suppresses routine console output emitted by
#'   `fdatest::ITP2bspline()`.
#'
#' @return A `windcut_fda_analysis` object with matrices, summaries,
#'   interval-test results, and tidy interval summaries.
#' @export
run_fda_analysis <- function(
    data,
    id_col,
    time_col,
    group_col,
    value_cols,
    value_labels = NULL,
    alpha = 0.05,
    n_permutations = 1000,
    smooth_method = c("spline", "lowess", "none"),
    smooth_args = list(),
    time_grid = NULL,
    n_time_grid = 300,
    reference_group = NULL,
    comparison_group = NULL,
    quiet = TRUE
) {
  smooth_method <- match.arg(smooth_method)

  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }

  required <- c(id_col, time_col, group_col, value_cols)
  missing_cols <- setdiff(required, names(data))
  if (length(missing_cols) > 0) {
    stop(sprintf("Missing required columns: %s", paste(missing_cols, collapse = ", ")), call. = FALSE)
  }

  if (!is.numeric(alpha) || length(alpha) < 1 || any(alpha <= 0 | alpha >= 1)) {
    stop("`alpha` must contain one or more values between 0 and 1.", call. = FALSE)
  }

  if (!is.numeric(n_permutations) || length(n_permutations) != 1 || n_permutations < 1) {
    stop("`n_permutations` must be a single positive number.", call. = FALSE)
  }

  if (is.null(time_grid)) {
    observed_time <- sort(unique(data[[time_col]]))
    time_grid <- seq(min(observed_time), max(observed_time), length.out = n_time_grid)
  }

  group_map <- .functional_group_map(data, id_col, group_col)
  groups <- .resolve_two_groups(group_map[[group_col]], reference_group, comparison_group)
  value_labels <- .resolve_value_labels(value_cols, value_labels)

  matrices <- list()
  summaries <- list()
  interval_tests <- list()
  interval_rows <- list()

  for (value_col in value_cols) {
    matrix_obj <- make_functional_matrix(
      data = data,
      id_col = id_col,
      time_col = time_col,
      value_col = value_col
    )

    aligned_groups <- group_map[[group_col]][match(matrix_obj$id, group_map[[id_col]])]
    group0 <- matrix_obj$y[aligned_groups == groups$reference, , drop = FALSE]
    group1 <- matrix_obj$y[aligned_groups == groups$comparison, , drop = FALSE]

    if (nrow(group0) == 0 || nrow(group1) == 0) {
      stop(sprintf("Both groups must have observations for `%s`.", value_col), call. = FALSE)
    }

    summary_obj <- functional_group_summary(
      data = data,
      id_col = id_col,
      time_col = time_col,
      value_col = value_col,
      group_col = group_col,
      time_grid = time_grid,
      method = smooth_method,
      smooth_args = smooth_args
    )

    tests_for_variable <- list()
    for (alpha_value in alpha) {
      test_obj <- functional_interval_test(
        group0 = group0,
        group1 = group1,
        n_permutations = n_permutations,
        alpha = alpha_value,
        time = matrix_obj$time,
        quiet = quiet
      )

      alpha_name <- .alpha_name(alpha_value)
      tests_for_variable[[alpha_name]] <- test_obj
      interval_rows[[length(interval_rows) + 1L]] <- .fda_interval_rows(
        intervals = test_obj$intervals,
        value_col = value_col,
        value_label = value_labels[[value_col]],
        alpha = alpha_value
      )
    }

    matrices[[value_col]] <- matrix_obj
    summaries[[value_col]] <- summary_obj
    interval_tests[[value_col]] <- tests_for_variable
  }

  interval_summary <- do.call(rbind, interval_rows)
  rownames(interval_summary) <- NULL

  out <- list(
    data = data,
    id_col = id_col,
    time_col = time_col,
    group_col = group_col,
    value_cols = value_cols,
    value_labels = value_labels,
    groups = groups,
    group_map = group_map,
    alpha = alpha,
    n_permutations = n_permutations,
    smooth_method = smooth_method,
    smooth_args = smooth_args,
    time_grid = time_grid,
    matrices = matrices,
    summaries = summaries,
    interval_tests = interval_tests,
    interval_summary = interval_summary
  )
  class(out) <- c("windcut_fda_analysis", "list")
  out
}

#' Plot FDA Mean Curves
#'
#' @param analysis A `windcut_fda_analysis` object returned by
#'   [run_fda_analysis()].
#' @param value_col Weather variable to plot.
#' @param title Plot title.
#' @param xlab Label for the x-axis.
#' @param ylab Label for the y-axis.
#' @param legend_title Legend title for the grouping variable.
#' @param palette Optional named vector of colors.
#'
#' @return A `ggplot2` object.
#' @export
plot_fda_means <- function(
    analysis,
    value_col,
    title = NULL,
    xlab = "Time",
    ylab = NULL,
    legend_title = "Group",
    palette = NULL
) {
  .check_fda_analysis(analysis)
  summary <- .fda_summary(analysis, value_col)

  if (is.null(title)) {
    title <- sprintf("Functional mean curves for %s", analysis$value_labels[[value_col]])
  }
  if (is.null(ylab)) {
    ylab <- analysis$value_labels[[value_col]]
  }

  plot_functional_means(
    summary,
    title = title,
    xlab = xlab,
    ylab = ylab,
    legend_title = legend_title,
    palette = palette
  )
}

#' Plot FDA Difference Curves
#'
#' @param analysis A `windcut_fda_analysis` object returned by
#'   [run_fda_analysis()].
#' @param value_col Weather variable to plot.
#' @param title Plot title.
#' @param xlab Label for the x-axis.
#' @param ylab Label for the y-axis.
#' @param color Line color.
#'
#' @return A `ggplot2` object.
#' @export
plot_fda_difference <- function(
    analysis,
    value_col,
    title = NULL,
    xlab = "Time",
    ylab = NULL,
    color = "#20262e"
) {
  .check_fda_analysis(analysis)
  summary <- .fda_summary(analysis, value_col)

  if (is.null(title)) {
    title <- sprintf("Functional difference for %s", analysis$value_labels[[value_col]])
  }
  if (is.null(ylab)) {
    ylab <- sprintf("%s minus %s", analysis$groups$comparison, analysis$groups$reference)
  }

  plot_functional_difference(
    summary,
    title = title,
    xlab = xlab,
    ylab = ylab,
    color = color
  )
}

#' Plot FDA Corrected P-Values
#'
#' @param analysis A `windcut_fda_analysis` object returned by
#'   [run_fda_analysis()].
#' @param value_col Weather variable to plot.
#' @param alpha Significance levels to show as horizontal reference lines. If
#'   `NULL`, all alpha values stored in `analysis` are shown.
#' @param title Plot title.
#' @param xlab Label for the x-axis.
#' @param ylab Label for the y-axis.
#'
#' @return A `ggplot2` object.
#' @export
plot_fda_p_values <- function(
    analysis,
    value_col,
    alpha = NULL,
    title = NULL,
    xlab = "Time",
    ylab = "Corrected p-value"
) {
  .check_fda_analysis(analysis)
  tests <- .fda_tests(analysis, value_col)

  if (is.null(alpha)) {
    alpha <- analysis$alpha
  }
  alpha_names <- .alpha_name(alpha)
  missing_alpha <- setdiff(alpha_names, names(tests))
  if (length(missing_alpha) > 0) {
    stop(sprintf("Alpha values were not found in `analysis`: %s", paste(alpha, collapse = ", ")), call. = FALSE)
  }

  plot_data <- tests[[alpha_names[[1]]]]$corrected_p_values
  if (is.null(title)) {
    title <- sprintf("Corrected p-values for %s", analysis$value_labels[[value_col]])
  }

  ggplot2::ggplot(plot_data, ggplot2::aes(time, corrected_p_value)) +
    ggplot2::geom_line(color = "#20262e", linewidth = 0.9) +
    ggplot2::geom_hline(
      data = data.frame(alpha = alpha),
      ggplot2::aes(yintercept = alpha),
      linetype = "dashed",
      color = "#c47f2c"
    ) +
    ggplot2::labs(title = title, x = xlab, y = ylab) +
    cowplot::theme_half_open()
}

#' Fit a Function-on-Scalar Model from an FDA Workflow
#'
#' Fits a binary-group function-on-scalar model for one variable stored in a
#' [run_fda_analysis()] result. The group contrast is created automatically from
#' the reference and comparison groups used in the FDA workflow.
#'
#' @param analysis A `windcut_fda_analysis` object returned by
#'   [run_fda_analysis()].
#' @param value_col Weather variable to model.
#' @param reference_score Numeric score assigned to the reference group.
#' @param comparison_score Numeric score assigned to the comparison group.
#' @param formula Formula passed to [functional_on_scalar()]. Defaults to
#'   `y ~ x`.
#' @param ... Additional arguments passed to [functional_on_scalar()].
#'
#' @return The fitted `pffr` model object.
#' @export
fit_fda_group_model <- function(
    analysis,
    value_col,
    reference_score = -1,
    comparison_score = 1,
    formula = y ~ x,
    ...
) {
  .check_fda_analysis(analysis)

  if (!value_col %in% names(analysis$matrices)) {
    stop(sprintf("`value_col` '%s' was not found in `analysis`.", value_col), call. = FALSE)
  }

  matrix_obj <- analysis$matrices[[value_col]]
  aligned_groups <- analysis$group_map[[analysis$group_col]][match(matrix_obj$id, analysis$group_map[[analysis$id_col]])]
  x <- ifelse(aligned_groups == analysis$groups$comparison, comparison_score, reference_score)

  functional_on_scalar(
    y = matrix_obj$y,
    x = x,
    yind = matrix_obj$time,
    formula = formula,
    ...
  )
}

#' Plot FDA Significant Intervals
#'
#' @param analysis A `windcut_fda_analysis` object returned by
#'   [run_fda_analysis()].
#' @param alpha Optional significance levels to plot. If `NULL`, all alpha
#'   values stored in `analysis` are shown.
#' @param show_none Logical; if `TRUE`, labels variables with no significant
#'   interval.
#' @param title Plot title.
#' @param xlab Label for the x-axis.
#'
#' @return A `ggplot2` object.
#' @export
plot_fda_intervals <- function(
    analysis,
    alpha = NULL,
    show_none = TRUE,
    title = "Significant functional intervals by weather variable",
    xlab = "Time"
) {
  .check_fda_analysis(analysis)

  plot_data <- analysis$interval_summary
  if (!is.null(alpha)) {
    plot_data <- plot_data[plot_data$alpha %in% alpha, , drop = FALSE]
  }

  if (nrow(plot_data) == 0) {
    stop("No interval rows are available for the requested alpha value.", call. = FALSE)
  }

  plot_data$variable_label <- factor(
    plot_data$variable_label,
    levels = rev(unique(analysis$value_labels[analysis$value_cols]))
  )
  plot_data$alpha_label <- factor(
    plot_data$alpha_label,
    levels = paste0("alpha = ", format(sort(unique(plot_data$alpha)), trim = TRUE))
  )

  p <- ggplot2::ggplot(plot_data, ggplot2::aes(y = variable_label)) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "gray75") +
    ggplot2::geom_segment(
      data = plot_data[!is.na(plot_data$start), , drop = FALSE],
      ggplot2::aes(x = start, xend = end, yend = variable_label),
      linewidth = 6,
      lineend = "butt",
      color = "#3b3b3b"
    ) +
    ggplot2::facet_wrap(~ alpha_label, ncol = 1) +
    ggplot2::labs(title = title, x = xlab, y = NULL) +
    cowplot::theme_half_open() +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank()
    )

  if (show_none) {
    none_rows <- plot_data[is.na(plot_data$start), c("variable_label", "alpha_label"), drop = FALSE]
    none_rows <- unique(none_rows)
    if (nrow(none_rows) > 0) {
      x_position <- min(analysis$time_grid, na.rm = TRUE) + diff(range(analysis$time_grid, na.rm = TRUE)) * 0.08
      p <- p + ggplot2::geom_text(
        data = none_rows,
        ggplot2::aes(x = x_position, y = variable_label, label = "no significant interval"),
        inherit.aes = FALSE,
        color = "gray45",
        size = 3.3
      )
    }
  }

  p
}

#' Extract Features from FDA Significant Intervals
#'
#' Converts significant FDA intervals into ordinary subject-level predictors for
#' downstream modeling.
#'
#' @param analysis A `windcut_fda_analysis` object returned by
#'   [run_fda_analysis()].
#' @param alpha Significance level used to select intervals. If `NULL`, the
#'   first alpha value stored in `analysis` is used.
#' @param value_cols Optional subset of weather variables to extract.
#' @param prefix Prefix used in feature names.
#' @param statistics Interval statistics to compute. Use function names such as
#'   `"mean"`, `"sd"`, and `"IQR"`, or a named list of custom functions.
#'
#' @return A wide data frame with one row per subject and one column per
#'   extracted FDA feature.
#' @export
extract_fda_features <- function(
    analysis,
    alpha = NULL,
    value_cols = NULL,
    prefix = "fda",
    statistics = "mean"
) {
  .check_fda_analysis(analysis)

  if (is.null(alpha)) {
    alpha <- analysis$alpha[[1]]
  }
  if (length(alpha) != 1) {
    stop("`alpha` must be a single value when extracting features.", call. = FALSE)
  }
  if (is.null(value_cols)) {
    value_cols <- analysis$value_cols
  }

  missing_values <- setdiff(value_cols, analysis$value_cols)
  if (length(missing_values) > 0) {
    stop(sprintf("Variables were not found in `analysis`: %s", paste(missing_values, collapse = ", ")), call. = FALSE)
  }

  ids <- unique(analysis$data[[analysis$id_col]])
  out <- data.frame(id = ids, stringsAsFactors = FALSE)
  names(out)[1] <- analysis$id_col

  interval_rows <- analysis$interval_summary
  interval_rows <- interval_rows[
    interval_rows$alpha == alpha & interval_rows$variable %in% value_cols & !is.na(interval_rows$start),
    ,
    drop = FALSE
  ]

  if (nrow(interval_rows) == 0) {
    return(out)
  }

  for (value_col in unique(interval_rows$variable)) {
    intervals <- interval_rows[interval_rows$variable == value_col, c("start", "end"), drop = FALSE]
    feature_prefix <- paste(prefix, value_col, sep = "_")
    features <- extract_interval_features(
      data = analysis$data,
      id_col = analysis$id_col,
      time_col = analysis$time_col,
      value_col = value_col,
      intervals = intervals,
      prefix = feature_prefix,
      statistics = statistics
    )
    out <- merge(out, features, by = analysis$id_col, all.x = TRUE, sort = FALSE)
  }

  out
}

#' Plot Smoothed Functional Means by Group
#'
#' Creates a `ggplot2` visualization from the output of
#' [functional_group_summary()]. The plotted values reflect the smoothing method
#' chosen when that summary object was created.
#'
#' @param summary A list returned by [functional_group_summary()] or a data frame
#'   with `group`, `time`, and `mean_value` columns.
#' @param title Plot title.
#' @param xlab Label for the x-axis.
#' @param ylab Label for the y-axis.
#' @param legend_title Legend title for the grouping variable.
#' @param palette Optional named vector of colors.
#' @param highlight_intervals Optional data frame with `start` and `end`
#'   columns used to shade one or more intervals.
#'
#' @return A `ggplot2` object.
#' @export
plot_functional_means <- function(
    summary,
    title = "Functional mean curves by group",
    xlab = "Time",
    ylab = "Mean value",
    legend_title = "Group",
    palette = NULL,
    highlight_intervals = NULL
) {
  plot_data <- if (is.list(summary)) summary$summary else summary

  required <- c("group", "time", "mean_value")
  missing_cols <- setdiff(required, names(plot_data))
  if (length(missing_cols) > 0) {
    stop(sprintf("Missing required columns: %s", paste(missing_cols, collapse = ", ")), call. = FALSE)
  }

  p <- ggplot2::ggplot(plot_data, ggplot2::aes(time, mean_value, color = group)) +
    .functional_highlight_layer(highlight_intervals) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::labs(
      title = title,
      x = xlab,
      y = ylab,
      color = legend_title
    ) +
    cowplot::theme_half_open()

  if (!is.null(palette)) {
    p <- p + ggplot2::scale_color_manual(values = palette)
  }

  p
}

#' Plot Functional Differences Between Two Groups
#'
#' Creates a `ggplot2` visualization from the difference table produced by
#' [functional_group_summary()]. The plotted values reflect the smoothing method
#' chosen when that summary object was created.
#'
#' @param summary A list returned by [functional_group_summary()] or a data frame
#'   with `time` and `difference` columns.
#' @param title Plot title.
#' @param xlab Label for the x-axis.
#' @param ylab Label for the y-axis.
#' @param color Line color.
#' @param highlight_intervals Optional data frame with `start` and `end`
#'   columns used to shade one or more intervals.
#'
#' @return A `ggplot2` object.
#' @export
plot_functional_difference <- function(
    summary,
    title = "Functional difference between groups",
    xlab = "Time",
    ylab = "Difference",
    color = "#20262e",
    highlight_intervals = NULL
) {
  plot_data <- if (is.list(summary)) summary$difference else summary

  if (is.null(plot_data)) {
    stop("A difference table is required to plot functional differences.", call. = FALSE)
  }

  required <- c("time", "difference")
  missing_cols <- setdiff(required, names(plot_data))
  if (length(missing_cols) > 0) {
    stop(sprintf("Missing required columns: %s", paste(missing_cols, collapse = ", ")), call. = FALSE)
  }

  ggplot2::ggplot(plot_data, ggplot2::aes(time, difference)) +
    .functional_highlight_layer(highlight_intervals) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "gray55") +
    ggplot2::geom_line(linewidth = 1, color = color) +
    ggplot2::labs(
      title = title,
      x = xlab,
      y = ylab
    ) +
    cowplot::theme_half_open()
}

#' Fit a Function-on-Scalar Regression
#'
#' Fit a Function-on-Scalar Regression with `refund::pffr()`
#'
#' Fits a function-on-scalar regression for cases where a scalar response or
#' contrast explains a functional weather series measured on a common grid.
#'
#' @param y A numeric matrix with subjects in rows and time values in columns.
#' @param x A numeric vector or data frame of scalar predictors.
#' @param yind Numeric vector of time values matching the matrix columns.
#' @param formula Formula for the scalar part of the model. Defaults to `y ~ x`.
#' @param ... Additional arguments passed to `refund::pffr()`.
#'
#' @return The fitted `pffr` model object.
#' @export
functional_on_scalar <- function(
    y,
    x,
    yind,
    formula = y ~ x,
    ...
) {
  if (!requireNamespace("refund", quietly = TRUE)) {
    stop("Package `refund` is required for `functional_on_scalar()`. Install it first.", call. = FALSE)
  }

  dat <- list(y = y, x = x, yind = yind)
  refund::pffr(formula = formula, yind = yind, data = dat, ...)
}

#' Plot Function-on-Scalar Coefficient Functions
#'
#' Extracts coefficient functions and approximate confidence bands from a fitted
#' `refund::pffr()` model and returns a faceted `ggplot2` visualization.
#'
#' @param model A fitted `pffr` model returned by [functional_on_scalar()] or
#'   directly by `refund::pffr()`.
#' @param ci_level Confidence level used to form approximate pointwise bands.
#' @param title Plot title.
#' @param xlab Label for the x-axis.
#' @param ylab Label for the y-axis.
#' @param highlight_intervals Optional data frame with `start` and `end`
#'   columns used to shade one or more intervals.
#'
#' @return A faceted `ggplot2` object.
#' @export
plot_functional_on_scalar <- function(
    model,
    ci_level = 0.95,
    title = "Function-on-scalar coefficient functions",
    xlab = "Time",
    ylab = "Coefficient function",
    highlight_intervals = NULL
) {
  smterms <- stats::coef(model)$smterms

  if (is.null(smterms) || length(smterms) == 0) {
    stop("No smooth coefficient terms were found in `model`.", call. = FALSE)
  }

  z_value <- stats::qnorm(0.5 + ci_level / 2)
  term_names <- names(smterms)
  if (is.null(term_names)) {
    term_names <- paste0("term_", seq_along(smterms))
  }

  rows <- lapply(seq_along(smterms), function(i) {
    coef_table <- smterms[[i]]$coef
    coef_names <- colnames(coef_table)
    time_values <- if (!is.null(coef_names) && "yindex.vec" %in% coef_names) {
      coef_table[, "yindex.vec"]
    } else {
      smterms[[i]]$x
    }
    estimates <- if (!is.null(coef_names) && "value" %in% coef_names) {
      coef_table[, "value"]
    } else {
      smterms[[i]]$value[, 1]
    }
    se_values <- if (!is.null(coef_names) && "se" %in% coef_names) {
      coef_table[, "se"]
    } else {
      smterms[[i]]$se
    }

    if (is.null(time_values) || is.null(estimates) || is.null(se_values)) {
      stop("The fitted model does not expose the coefficient information needed for plotting.", call. = FALSE)
    }

    data.frame(
      term = term_names[[i]],
      time = time_values,
      estimate = estimates,
      lower = estimates - z_value * se_values,
      upper = estimates + z_value * se_values,
      stringsAsFactors = FALSE
    )
  })

  plot_data <- do.call(rbind, rows)

  ggplot2::ggplot(plot_data, ggplot2::aes(time, estimate)) +
    .functional_highlight_layer(highlight_intervals) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = lower, ymax = upper), alpha = 0.2, fill = "#6ea87d") +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "gray55") +
    ggplot2::geom_line(linewidth = 1, color = "#2b6c4f") +
    ggplot2::facet_grid(term ~ ., scales = "free_y") +
    ggplot2::labs(
      title = title,
      x = xlab,
      y = ylab
    ) +
    cowplot::theme_half_open()
}

#' Run an Interval-Wise Functional Test
#'
#' Run `fdatest::ITP2bspline()` on Two Functional Groups
#'
#' Runs `fdatest::ITP2bspline()` to compare two groups of functional
#' observations evaluated on the same grid.
#'
#' @param group0 Numeric matrix for the reference group.
#' @param group1 Numeric matrix for the comparison group.
#' @param n_permutations Number of permutations used by the interval-wise test.
#' @param alpha Significance level used when extracting intervals.
#' @param time Numeric vector of time values corresponding to matrix columns.
#' @param quiet Logical; if `TRUE`, suppresses routine console output emitted by
#'   `fdatest::ITP2bspline()`.
#'
#' @return A list with the raw test object, a table of corrected p-values by
#'   time value, the selected significance level, the significant time
#'   values, and a table of collapsed significant intervals.
#' @export
functional_interval_test <- function(
    group0,
    group1,
    n_permutations = 1000,
    alpha = 0.05,
    time = NULL,
    quiet = TRUE
) {
  if (!requireNamespace("fdatest", quietly = TRUE)) {
    stop("Package `fdatest` is required for `functional_interval_test()`. Install it first.", call. = FALSE)
  }

  if (quiet) {
    invisible(utils::capture.output(
      result <- fdatest::ITP2bspline(group0, group1, B = n_permutations)
    ))
  } else {
    result <- fdatest::ITP2bspline(group0, group1, B = n_permutations)
  }

  pvals <- result$corrected.pval

  if (is.null(time)) {
    time <- seq_along(pvals)
  }

  significant <- time[which(pvals < alpha)]
  intervals <- .collapse_consecutive_intervals(significant)
  corrected_p_values <- data.frame(
    time = time,
    corrected_p_value = pvals,
    significant = pvals < alpha,
    stringsAsFactors = FALSE
  )

  list(
    test = result,
    corrected_p_values = corrected_p_values,
    alpha = alpha,
    significant_time = significant,
    intervals = intervals
  )
}

#' Extract Features from Functional Intervals
#'
#' Uses one or more time intervals to compute subject-level summaries from a
#' long-format weather series. This is useful after interval-wise testing has
#' identified periods of functional divergence.
#'
#' @param data A data frame in long format.
#' @param id_col Subject identifier column.
#' @param time_col Time column, such as relative day or days after planting.
#' @param value_col Measured series column.
#' @param intervals A data frame with `start` and `end` columns.
#' @param prefix Prefix used when naming the resulting features.
#' @param statistics Interval statistics to compute. Use function names such as
#'   `"mean"`, `"sd"`, and `"IQR"`, or a named list of custom functions.
#'
#' @return A wide data frame with one row per subject and one column per
#'   interval-derived feature.
#' @export
extract_interval_features <- function(
    data,
    id_col,
    time_col,
    value_col,
    intervals,
    prefix = NULL,
    statistics = "mean"
) {
  if (!all(c("start", "end") %in% names(intervals))) {
    stop("`intervals` must contain `start` and `end` columns.", call. = FALSE)
  }
  statistics <- .resolve_interval_statistics(statistics)

  ids <- unique(data[[id_col]])
  out <- data.frame(id = ids, stringsAsFactors = FALSE)
  names(out)[1] <- id_col

  base_prefix <- if (is.null(prefix)) value_col else prefix

  for (i in seq_len(nrow(intervals))) {
    start <- intervals$start[[i]]
    end <- intervals$end[[i]]

    subset <- data[data[[time_col]] >= start & data[[time_col]] <= end, , drop = FALSE]

    for (statistic_name in names(statistics)) {
      feature_name <- sprintf("%s_%s_%s_%s", base_prefix, statistic_name, start, end)
      values <- stats::aggregate(
        subset[[value_col]],
        by = list(subset[[id_col]]),
        FUN = function(z) .compute_window_statistic(z, statistics[[statistic_name]])
      )

      names(values) <- c(id_col, feature_name)
      out <- merge(out, values, by = id_col, all.x = TRUE, sort = FALSE)
    }
  }

  out
}

.resolve_interval_statistics <- function(statistics) {
  .resolve_statistic_spec(statistics)
}

.collapse_consecutive_intervals <- function(values) {
  if (length(values) == 0) {
    return(data.frame(start = numeric(), end = numeric()))
  }

  groups <- split(values, cumsum(c(1, diff(values) != 1)))
  data.frame(
    start = vapply(groups, function(x) x[[1]], numeric(1)),
    end = vapply(groups, function(x) x[[length(x)]], numeric(1))
  )
}

.smooth_functional_values <- function(time, values, xout = time, method, smooth_args) {
  complete <- stats::complete.cases(time, values)
  if (sum(complete) < 2) {
    return(rep(NA_real_, length(xout)))
  }

  if (identical(method, "none")) {
    return(stats::approx(time[complete], values[complete], xout = xout, rule = 2)$y)
  }

  time_complete <- time[complete]
  values_complete <- values[complete]

  if (identical(method, "lowess")) {
    f <- if (!is.null(smooth_args$f)) smooth_args$f else 0.2
    iter <- if (!is.null(smooth_args$iter)) smooth_args$iter else 3
    fit <- stats::lowess(x = time_complete, y = values_complete, f = f, iter = iter)
    smoothed <- stats::approx(fit$x, fit$y, xout = xout, rule = 2)$y
    return(smoothed)
  }

  spline_args <- c(
    list(x = time_complete, y = values_complete),
    smooth_args
  )
  fit <- do.call(stats::smooth.spline, spline_args)
  stats::predict(fit, x = xout)$y
}

.functional_highlight_layer <- function(highlight_intervals) {
  if (is.null(highlight_intervals)) {
    return(ggplot2::geom_blank())
  }

  if (!all(c("start", "end") %in% names(highlight_intervals))) {
    stop("`highlight_intervals` must contain `start` and `end` columns.", call. = FALSE)
  }

  ggplot2::geom_rect(
    data = highlight_intervals,
    ggplot2::aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf),
    inherit.aes = FALSE,
    fill = "#4b79a8",
    alpha = 0.12
  )
}

.functional_group_map <- function(data, id_col, group_col) {
  group_pairs <- as.data.frame(unique(data[c(id_col, group_col)]), stringsAsFactors = FALSE)
  counts <- stats::aggregate(
    group_pairs[[group_col]],
    by = list(group_pairs[[id_col]]),
    FUN = function(x) length(unique(x))
  )
  names(counts) <- c(id_col, "n_groups")
  ambiguous <- counts[[id_col]][counts$n_groups > 1]
  if (length(ambiguous) > 0) {
    stop(
      sprintf("Each subject must belong to only one group. Ambiguous subjects: %s", paste(ambiguous, collapse = ", ")),
      call. = FALSE
    )
  }

  group_pairs
}

.resolve_two_groups <- function(group_values, reference_group, comparison_group) {
  groups <- if (is.factor(group_values)) {
    levels(group_values)[levels(group_values) %in% as.character(unique(group_values))]
  } else {
    unique(group_values)
  }

  if (length(groups) != 2) {
    stop("`group_col` must contain exactly two groups for FDA interval testing.", call. = FALSE)
  }

  if (is.null(reference_group)) {
    reference_group <- groups[[1]]
  }
  if (is.null(comparison_group)) {
    comparison_group <- setdiff(groups, reference_group)[[1]]
  }

  if (!reference_group %in% groups) {
    stop("`reference_group` was not found in `group_col`.", call. = FALSE)
  }
  if (!comparison_group %in% groups) {
    stop("`comparison_group` was not found in `group_col`.", call. = FALSE)
  }
  if (identical(reference_group, comparison_group)) {
    stop("`reference_group` and `comparison_group` must be different.", call. = FALSE)
  }

  list(reference = reference_group, comparison = comparison_group)
}

.resolve_value_labels <- function(value_cols, value_labels) {
  labels <- stats::setNames(value_cols, value_cols)

  if (is.null(value_labels)) {
    return(labels)
  }

  if (is.data.frame(value_labels)) {
    if (!all(c("variable", "label") %in% names(value_labels))) {
      stop("`value_labels` data frames must contain `variable` and `label` columns.", call. = FALSE)
    }
    provided <- stats::setNames(as.character(value_labels$label), as.character(value_labels$variable))
  } else {
    provided_names <- names(value_labels)
    provided <- as.character(value_labels)
    names(provided) <- provided_names
    if (is.null(provided_names) || any(provided_names == "")) {
      stop("`value_labels` must be named when supplied as a vector.", call. = FALSE)
    }
  }

  labels[names(provided)] <- provided
  labels
}

.alpha_name <- function(alpha) {
  paste0("alpha_", gsub("[^0-9]+", "_", format(alpha, trim = TRUE)))
}

.fda_interval_rows <- function(intervals, value_col, value_label, alpha) {
  alpha_label <- paste0("alpha = ", format(alpha, trim = TRUE))

  if (nrow(intervals) == 0) {
    return(data.frame(
      variable = value_col,
      variable_label = value_label,
      alpha = alpha,
      alpha_label = alpha_label,
      start = NA_real_,
      end = NA_real_,
      status = "No significant interval",
      stringsAsFactors = FALSE
    ))
  }

  data.frame(
    variable = value_col,
    variable_label = value_label,
    alpha = alpha,
    alpha_label = alpha_label,
    start = intervals$start,
    end = intervals$end,
    status = "Significant interval",
    stringsAsFactors = FALSE
  )
}

.check_fda_analysis <- function(analysis) {
  if (!inherits(analysis, "windcut_fda_analysis")) {
    stop("`analysis` must be an object returned by `run_fda_analysis()`.", call. = FALSE)
  }
}

.fda_summary <- function(analysis, value_col) {
  if (!value_col %in% names(analysis$summaries)) {
    stop(sprintf("`value_col` '%s' was not found in `analysis`.", value_col), call. = FALSE)
  }
  analysis$summaries[[value_col]]
}

.fda_tests <- function(analysis, value_col) {
  if (!value_col %in% names(analysis$interval_tests)) {
    stop(sprintf("`value_col` '%s' was not found in `analysis`.", value_col), call. = FALSE)
  }
  analysis$interval_tests[[value_col]]
}

