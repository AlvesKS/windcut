#' Demo Data for Window-Pane Workflows
#'
#' A bundled dataset for the getting-started, window-pane, and feature-screening
#' tutorials. The object is a named list with two components:
#'
#' - `weather`: daily weather data for multiple simulated sites, all with the
#'   same time-series length and at least 30 days before planting.
#' - `weather_hourly`: hourly weather data used to create `weather`.
#' - `assessments`: one disease assessment per site, including
#'   `assessment_time`, `planting_time`, and `disease_intensity`. Planting dates
#'   vary by site, and assessments occur about 90 days after planting.
#'
#' The data were generated with [simulate_weather_series()] and
#' [simulate_assessment_data()] and then saved as package data so the main
#' workflow tutorials can begin directly with analysis.
#'
#' @format A list with components `weather`, `weather_hourly`, and
#'   `assessments`.
#' @name window_pane_demo_data
#' @aliases window_pane_demo_data
"window_pane_demo_data"

#' Demo Data for Functional Data Analysis Workflows
#'
#' A bundled dataset for the functional data analysis tutorial. The object is a
#' named list with three components:
#'
#' - `weather_daily`: daily weather curves for multiple simulated sites.
#' - `assessments`: one binary disease assessment per site with `wm` and
#'   `wm_class`.
#' - `variable_specs`: a small lookup table of FDA variables and display labels.
#'
#' The data were created from the package's simulation functions and then bundled
#' so the FDA vignette can focus on interpretation, smoothing, interval testing,
#' and feature extraction.
#'
#' @format A list with components `weather_daily`, `assessments`, and
#'   `variable_specs`.
#' @name fda_demo_data
#' @aliases fda_demo_data
"fda_demo_data"

