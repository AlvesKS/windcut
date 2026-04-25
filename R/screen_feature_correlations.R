#' Screen Pairwise Correlations Among Candidate Features
#'
#' Computes pairwise correlations among numeric candidate predictors and suggests
#' a reduced set of variables for modeling workflows that should avoid strongly
#' correlated features. This is useful after [window_pane()] creates many
#' metric-window columns.
#'
#' The selection heuristic is intentionally simple and transparent. For every
#' pair with absolute correlation greater than or equal to `threshold`, the
#' function removes the variable with the larger mean absolute correlation to
#' all other candidate variables. This favors keeping variables that are less
#' redundant overall.
#'
#' @param data A data frame containing candidate feature columns.
#' @param feature_cols Optional character vector of feature columns to compare.
#'   If `NULL`, all numeric columns are considered except `exclude_cols`.
#' @param exclude_cols Columns to exclude when `feature_cols = NULL`, such as
#'   identifiers, assessment times, or response variables.
#' @param method Correlation method passed to [stats::cor()]. One of
#'   `"pearson"`, `"spearman"`, or `"kendall"`.
#' @param threshold Absolute correlation threshold used to flag highly
#'   correlated feature pairs.
#' @param use Missing-data handling passed to [stats::cor()].
#'
#' @return A list with:
#' \describe{
#'   \item{correlation_matrix}{Feature-by-feature correlation matrix.}
#'   \item{high_correlations}{Data frame of feature pairs with absolute
#'   correlation greater than or equal to `threshold`.}
#'   \item{suggested_features}{Character vector of features suggested to keep.}
#'   \item{removed_features}{Character vector of features suggested for removal.}
#'   \item{feature_summary}{Data frame with each feature's mean absolute
#'   correlation and suggested decision.}
#' }
#'
#' @examples
#' weather <- simulate_weather_series(
#'   days = 40,
#'   n_series = 8,
#'   id_col = "site_id",
#'   seed = 1
#' )
#' assessments <- simulate_assessment_data(weather, id_col = "site_id", seed = 1)
#' windows <- make_windows(min_offset = -10, max_offset = -1, width = 3)
#' features <- window_pane(
#'   weather = weather,
#'   assessments = assessments,
#'   windows = windows,
#'   reference_col = "assessment_time",
#'   id_col = "site_id",
#'   response_col = "disease_intensity"
#' )
#'
#' correlation_screen <- screen_feature_correlations(
#'   features,
#'   exclude_cols = c("site_id", "assessment_time", "disease_intensity"),
#'   method = "spearman",
#'   threshold = 0.8
#' )
#' correlation_screen$suggested_features
#'
#' @export
screen_feature_correlations <- function(
    data,
    feature_cols = NULL,
    exclude_cols = NULL,
    method = c("pearson", "spearman", "kendall"),
    threshold = 0.8,
    use = "pairwise.complete.obs"
) {
  method <- match.arg(method)

  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }

  if (!is.numeric(threshold) || length(threshold) != 1 || threshold < 0 || threshold > 1) {
    stop("`threshold` must be a single number between 0 and 1.", call. = FALSE)
  }

  if (is.null(feature_cols)) {
    numeric_cols <- names(data)[vapply(data, is.numeric, logical(1))]
    feature_cols <- setdiff(numeric_cols, exclude_cols)
  }

  missing_cols <- setdiff(feature_cols, names(data))
  if (length(missing_cols) > 0) {
    stop(
      sprintf("Missing feature columns: %s", paste(missing_cols, collapse = ", ")),
      call. = FALSE
    )
  }

  non_numeric <- feature_cols[!vapply(data[feature_cols], is.numeric, logical(1))]
  if (length(non_numeric) > 0) {
    stop(
      sprintf("Feature columns must be numeric: %s", paste(non_numeric, collapse = ", ")),
      call. = FALSE
    )
  }

  if (length(feature_cols) < 2) {
    stop("At least two numeric feature columns are required.", call. = FALSE)
  }

  feature_data <- data[feature_cols]
  constant_features <- feature_cols[vapply(feature_data, function(x) {
    complete <- x[!is.na(x)]
    length(unique(complete)) < 2
  }, logical(1))]

  cor_matrix <- suppressWarnings(stats::cor(
    feature_data,
    method = method,
    use = use
  ))

  high_correlations <- .high_correlation_pairs(cor_matrix, threshold)
  mean_abs <- .mean_abs_correlations(cor_matrix)
  removed <- .choose_correlated_features_to_remove(high_correlations, mean_abs)
  removed <- unique(c(removed, constant_features))
  suggested <- setdiff(feature_cols, removed)

  feature_summary <- data.frame(
    feature = feature_cols,
    mean_abs_correlation = unname(mean_abs[feature_cols]),
    suggested = feature_cols %in% suggested,
    reason = ifelse(
      feature_cols %in% constant_features,
      "constant_or_near_constant",
      ifelse(feature_cols %in% removed, "highly_correlated", "keep")
    ),
    stringsAsFactors = FALSE
  )
  feature_summary <- feature_summary[order(!feature_summary$suggested, feature_summary$mean_abs_correlation), , drop = FALSE]
  rownames(feature_summary) <- NULL

  list(
    correlation_matrix = cor_matrix,
    high_correlations = high_correlations,
    suggested_features = suggested,
    removed_features = removed,
    feature_summary = feature_summary,
    method = method,
    threshold = threshold
  )
}

.high_correlation_pairs <- function(cor_matrix, threshold) {
  if (ncol(cor_matrix) < 2) {
    return(data.frame(
      feature_a = character(),
      feature_b = character(),
      correlation = numeric(),
      abs_correlation = numeric(),
      stringsAsFactors = FALSE
    ))
  }

  upper <- which(upper.tri(cor_matrix), arr.ind = TRUE)
  pairs <- data.frame(
    feature_a = rownames(cor_matrix)[upper[, "row"]],
    feature_b = colnames(cor_matrix)[upper[, "col"]],
    correlation = cor_matrix[upper],
    stringsAsFactors = FALSE
  )
  pairs$abs_correlation <- abs(pairs$correlation)
  pairs <- pairs[!is.na(pairs$abs_correlation) & pairs$abs_correlation >= threshold, , drop = FALSE]
  pairs <- pairs[order(pairs$abs_correlation, decreasing = TRUE), , drop = FALSE]
  rownames(pairs) <- NULL
  pairs
}

.mean_abs_correlations <- function(cor_matrix) {
  abs_cor <- abs(cor_matrix)
  diag(abs_cor) <- NA_real_
  out <- rowMeans(abs_cor, na.rm = TRUE)
  out[is.nan(out)] <- NA_real_
  out
}

.choose_correlated_features_to_remove <- function(high_correlations, mean_abs) {
  removed <- character()

  if (nrow(high_correlations) == 0) {
    return(removed)
  }

  for (i in seq_len(nrow(high_correlations))) {
    a <- high_correlations$feature_a[[i]]
    b <- high_correlations$feature_b[[i]]

    if (a %in% removed || b %in% removed) {
      next
    }

    score_a <- mean_abs[[a]]
    score_b <- mean_abs[[b]]
    if (is.na(score_a)) {
      score_a <- Inf
    }
    if (is.na(score_b)) {
      score_b <- Inf
    }

    if (score_a > score_b) {
      removed <- c(removed, a)
    } else if (score_b > score_a) {
      removed <- c(removed, b)
    } else {
      removed <- c(removed, sort(c(a, b))[[2]])
    }
  }

  unique(removed)
}

