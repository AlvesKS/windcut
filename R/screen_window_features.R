#' Screen Window-Derived Features Against a Disease Response
#'
#' Computes univariate associations between weather features produced by
#' [window_pane()] and a disease response. This implements the screening step
#' commonly used after window-pane feature construction, where each metric-window
#' summary is related to the response using a correlation coefficient and the
#' resulting p-values are adjusted for multiple testing.
#'
#' @param data A data frame that includes the response column and feature columns.
#' @param response_col Name of the response column.
#' @param feature_cols Optional character vector of feature columns to test. If
#'   `NULL`, all numeric columns except the response are screened.
#' @param method Correlation method passed to [stats::cor.test()]. One of
#'   `"spearman"` or `"pearson"`.
#' @param adjust_method Method used by [stats::p.adjust()] to correct p-values.
#'
#' @return A data frame with feature names, parsed metric/window labels,
#'   correlation estimates, raw p-values, adjusted p-values, and sample size.
#' @export
screen_window_features <- function(
    data,
    response_col,
    feature_cols = NULL,
    method = c("spearman", "pearson"),
    adjust_method = "BH"
) {
  method <- match.arg(method)

  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }

  if (!response_col %in% names(data)) {
    stop(sprintf("`response_col` '%s' was not found.", response_col), call. = FALSE)
  }

  response <- data[[response_col]]
  if (!is.numeric(response)) {
    stop("`response_col` must be numeric for correlation screening.", call. = FALSE)
  }

  if (is.null(feature_cols)) {
    numeric_cols <- names(data)[vapply(data, is.numeric, logical(1))]
    feature_cols <- setdiff(numeric_cols, response_col)
  }

  if (length(feature_cols) == 0) {
    return(data.frame(
      feature = character(),
      metric = character(),
      window = character(),
      estimate = numeric(),
      p_value = numeric(),
      p_adjusted = numeric(),
      n_complete = integer(),
      stringsAsFactors = FALSE
    ))
  }

  rows <- lapply(feature_cols, function(feature) {
    x <- data[[feature]]
    complete <- stats::complete.cases(x, response)
    n_complete <- sum(complete)

    metric <- sub("^(.*)_window_[mpz]\\d+_[mpz]\\d+$", "\\1", feature)
    window <- sub("^.*_(window_[mpz]\\d+_[mpz]\\d+)$", "\\1", feature)
    if (identical(metric, feature)) {
      metric <- NA_character_
      window <- NA_character_
    }

    if (n_complete < 3 || length(unique(x[complete])) < 2 || length(unique(response[complete])) < 2) {
      return(data.frame(
        feature = feature,
        metric = metric,
        window = window,
        estimate = NA_real_,
        p_value = NA_real_,
        n_complete = n_complete,
        stringsAsFactors = FALSE
      ))
    }

    test <- suppressWarnings(stats::cor.test(x[complete], response[complete], method = method))

    data.frame(
      feature = feature,
      metric = metric,
      window = window,
      estimate = unname(test$estimate),
      p_value = test$p.value,
      n_complete = n_complete,
      stringsAsFactors = FALSE
    )
  })

  out <- do.call(rbind, rows)
  out$p_adjusted <- stats::p.adjust(out$p_value, method = adjust_method)
  out <- out[order(abs(out$estimate), decreasing = TRUE, na.last = TRUE), , drop = FALSE]
  rownames(out) <- NULL
  out
}

