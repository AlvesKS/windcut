#' Plot a Window-Pane Grid
#'
#' Visualizes candidate relative-time windows created by [make_windows()]. The
#' vertical dashed line marks the reference date. Windows fully before the
#' reference, fully after the reference, and crossing the reference are colored
#' separately so the timing logic is visible.
#'
#' @param windows A data frame created by [make_windows()].
#' @param max_windows Maximum number of windows to display. Use `Inf` to show
#'   all rows.
#' @param color_by Character value controlling segment color. Use `"timing"` to
#'   color windows by position relative to the reference date, `"width"` to
#'   color by window duration, or `"none"` for one color.
#' @param title Plot title.
#' @param subtitle Plot subtitle. If `NULL`, a subtitle is generated from the
#'   selected `color_by` value.
#' @param xlab X-axis label.
#' @param ylab Y-axis label.
#' @param reference_label Label used for the vertical line at relative-time 0.
#'
#' @return A `ggplot2` object.
#' @examples
#' windows <- make_windows(min_offset = -14, max_offset = 35, width = 7, slide_by = 7)
#' plot_window_pane(windows)
#'
#' variable_windows <- make_windows(min_offset = -21, max_offset = -1, width = c(3, 5, 7))
#' plot_window_pane(variable_windows, color_by = "width")
#'
#' @export
plot_window_pane <- function(
    windows,
    max_windows = 30,
    color_by = c("timing", "width", "none"),
    title = "Window-pane grid",
    subtitle = NULL,
    xlab = "Time relative to reference date",
    ylab = "Candidate window",
    reference_label = "Reference date"
) {
  color_by <- match.arg(color_by)

  required_cols <- c("relative_start", "relative_end", "width", "label")
  missing_cols <- setdiff(required_cols, names(windows))
  if (length(missing_cols) > 0) {
    stop(
      sprintf("`windows` must contain: %s.", paste(missing_cols, collapse = ", ")),
      call. = FALSE
    )
  }

  if (!is.numeric(max_windows) || length(max_windows) != 1 || is.na(max_windows) || max_windows <= 0) {
    stop("`max_windows` must be a positive number or Inf.", call. = FALSE)
  }

  plot_data <- windows
  if (is.finite(max_windows)) {
    plot_data <- utils::head(plot_data, max_windows)
  }

  if (nrow(plot_data) == 0) {
    stop("`windows` must contain at least one row to plot.", call. = FALSE)
  }

  plot_data$label <- factor(plot_data$label, levels = rev(plot_data$label))
  plot_data$timing <- .window_timing_label(plot_data$relative_start, plot_data$relative_end)

  if (is.null(subtitle)) {
    subtitle <- switch(
      color_by,
      timing = "Negative offsets are before the reference; positive offsets are after it",
      width = "Color shows window duration",
      none = reference_label
    )
  }

  x_limits <- range(c(0, plot_data$relative_start, plot_data$relative_end), na.rm = TRUE)
  x_padding <- max(1, diff(x_limits) * 0.04)
  x_limits <- x_limits + c(-x_padding, x_padding)

  p <- ggplot2::ggplot(plot_data, ggplot2::aes(y = label)) +
    ggplot2::geom_vline(
      xintercept = 0,
      linetype = "dashed",
      color = "#20262e",
      linewidth = 0.8
    ) +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      x = xlab,
      y = ylab
    ) +
    ggplot2::scale_x_continuous(limits = x_limits) +
    cowplot::theme_half_open()

  if (identical(color_by, "timing")) {
    p <- p +
      ggplot2::geom_segment(
        ggplot2::aes(
          x = relative_start,
          xend = relative_end,
          yend = label,
          color = timing
        ),
        linewidth = 2.2
      ) +
      ggplot2::scale_color_manual(
        values = c(
          "before reference" = "#c47f2c",
          "crosses reference" = "#6ea87d",
          "after reference" = "#3f7d58"
        ),
        name = NULL
      )
  } else if (identical(color_by, "width")) {
    width_levels <- sort(unique(as.character(plot_data$width)))
    p <- p +
      ggplot2::geom_segment(
        ggplot2::aes(
          x = relative_start,
          xend = relative_end,
          yend = label,
          color = factor(width)
        ),
        linewidth = 2.2
      ) +
      ggplot2::scale_color_manual(
        values = stats::setNames(.window_width_palette(length(width_levels)), width_levels),
        name = "Width"
      )
  } else {
    p <- p +
      ggplot2::geom_segment(
        ggplot2::aes(x = relative_start, xend = relative_end, yend = label),
        color = "#3f7d58",
        linewidth = 2.2
      )
  }

  p
}

.window_timing_label <- function(relative_start, relative_end) {
  out <- rep("crosses reference", length(relative_start))
  out[relative_end < 0] <- "before reference"
  out[relative_start > 0] <- "after reference"
  factor(out, levels = c("before reference", "crosses reference", "after reference"))
}

.window_width_palette <- function(n) {
  base_colors <- c("#2b6c4f", "#6ea87d", "#d9a441", "#c47f2c", "#8a5a44", "#5f6f94")
  if (n <= length(base_colors)) {
    return(base_colors[seq_len(n)])
  }
  grDevices::colorRampPalette(base_colors)(n)
}
