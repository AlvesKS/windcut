#' Create Candidate Relative-Time Windows
#'
#' Generates windows relative to a reference date. Negative offsets are before
#' the reference date, zero is the reference date, and positive offsets are
#' after the reference date. Use fixed-width windows for the classic window-pane
#' scan, where one exposure duration slides through the relative-time range. Use
#' variable-width windows when you also want to scan alternative exposure
#' durations.
#'
#' @param min_offset Minimum relative-time offset included in the grid.
#' @param max_offset Maximum relative-time offset included in the grid.
#' @param width Window duration in relative-time units. For example, with
#'   `min_offset = -5`, `max_offset = 0`, and `width = 5`, the window is labeled
#'   `window_m05_z00` and covers the five relative-time units ending at the
#'   reference date. Use one value for a fixed-duration scan, such as
#'   `width = 5`. Use several values for a variable-duration scan, such as
#'   `width = 2:4` or `width = c(2, 4, 6)`.
#' @param slide_by Number of relative-time units used when sliding the window
#'   start. A value of `1` creates consecutive starts; a value of `2` skips
#'   every other possible start.
#' @param type Window-generation strategy. `"fixed"` creates one sliding
#'   window width. `"variable"` creates windows from all values in `width`. If
#'   `width` has more than one value, `type` is treated as `"variable"`.
#' @param reference_col Optional name of the reference timestamp column, such
#'   as `"assessment_time"` or `"planting_time"`. This is stored as metadata on
#'   the returned window grid and used by [window_pane()] when its own
#'   `reference_col` argument is `NULL`.
#'
#' @return A data frame with `relative_start`, `relative_end`, `width`, and
#'   `label`.
#'
#' @examples
#' make_windows(min_offset = -21, max_offset = -1, width = 5)
#'
#' make_windows(min_offset = 1, max_offset = 21, width = 5, slide_by = 3)
#'
#' make_windows(min_offset = -7, max_offset = -1, width = 5, reference_col = "planting_time")
#'
#' make_windows(
#'   min_offset = -5,
#'   max_offset = 5,
#'   width = c(2, 4, 6),
#'   slide_by = 2
#' )
#'
#' @export
make_windows <- function(
    min_offset = -30,
    max_offset = -1,
    width = 5,
    slide_by = 1,
    type = c("fixed", "variable"),
    reference_col = NULL
) {
  type <- match.arg(type)

  if (!is.null(reference_col) && (!is.character(reference_col) || length(reference_col) != 1)) {
    stop("`reference_col` must be a single character string or NULL.", call. = FALSE)
  }

  if (!is.numeric(width) || length(width) == 0 || any(!is.finite(width))) {
    stop("`width` must be a non-empty numeric vector.", call. = FALSE)
  }
  if (any(width <= 0)) {
    stop("`width` must contain values greater than zero.", call. = FALSE)
  }
  if (any(width != floor(width))) {
    stop("`width` must contain whole-number values.", call. = FALSE)
  }

  if (identical(type, "fixed") && length(width) > 1) {
    type <- "variable"
  }

  args <- c(min_offset, max_offset, slide_by)
  if (any(!is.numeric(args)) || any(!is.finite(args))) {
    stop("Window parameters must be finite numeric values.", call. = FALSE)
  }

  if (min_offset > max_offset) {
    stop("`min_offset` must be less than or equal to `max_offset`.", call. = FALSE)
  }

  if (slide_by <= 0) {
    stop("`slide_by` must be greater than zero.", call. = FALSE)
  }

  if (identical(type, "fixed")) {
    candidate_widths <- width
  } else {
    candidate_widths <- sort(unique(width))
  }

  rows <- list()
  index <- 1L

  for (relative_start in seq.int(min_offset, max_offset, by = slide_by)) {
    for (current_width in candidate_widths) {
      relative_end <- relative_start + current_width
      if (relative_end <= max_offset) {
        rows[[index]] <- data.frame(
          relative_start = relative_start,
          relative_end = relative_end,
          width = current_width,
          label = .format_window_label(relative_start, relative_end),
          stringsAsFactors = FALSE
        )
        index <- index + 1L
      }
    }
  }

  if (length(rows) == 0) {
    out <- data.frame(
      relative_start = numeric(),
      relative_end = numeric(),
      width = numeric(),
      label = character(),
      stringsAsFactors = FALSE
    )
    attr(out, "reference_col") <- reference_col
    return(out)
  }

  out <- do.call(rbind, rows)
  attr(out, "reference_col") <- reference_col
  out
}

.format_window_label <- function(relative_start, relative_end) {
  paste(
    "window",
    .format_offset(relative_start),
    .format_offset(relative_end),
    sep = "_"
  )
}

.format_offset <- function(x) {
  prefix <- ifelse(x < 0, "m", ifelse(x > 0, "p", "z"))
  sprintf("%s%02d", prefix, abs(x))
}

