#' windcut: Weather Time Series Feature Engineering for Plant Disease Epidemiology
#'
#' `windcut` provides functions to validate weather series, generate candidate
#' relative-time windows, summarise weather inside those windows, simulate
#' weather and assessment datasets, and create model-ready feature tables for
#' plant disease epidemiology workflows.
#'
#' @keywords internal
"_PACKAGE"

utils::globalVariables(c(
  "corrected_p_value",
  "difference",
  "end",
  "estimate",
  "group",
  "label",
  "lower",
  "mean_value",
  "relative_end",
  "relative_start",
  "start",
  "time",
  "timing",
  "upper",
  "variable_label",
  "width"
))

