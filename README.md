# windcut

`windcut` is an R package for plant disease epidemiologists who need to turn
raw weather time series into model-ready predictors. The first version focuses
on relative-time weather windows: generate candidate windows before, after, or
around a biological reference date, summarise the weather inside each window,
and return a wide feature table ready for modeling.

The package website can be built locally with:

```r
source("scripts/build_site.R")
```

## Why `windcut`?

Disease intensity is often driven by weather in biologically meaningful periods
that are not known in advance. Instead of choosing one arbitrary period, you
can scan many windows relative to each reference date and ask which combination
of timing and weather variables is most informative.

Windows can be generated in two ways: fixed-width windows that slide through
time, such as `window_m05_z00`, `window_m04_p01`, and `window_m03_p02`, or
variable-width windows that also compare different exposure durations. In these
labels, `m` means before the reference date, `z` means the reference date, and
`p` means after the reference date.

## Core workflow

1. Validate and standardise a weather time series.
2. Generate candidate windows with `make_windows()`.
3. Compute weather summaries per window with `scan_windows()`.
4. Expand those summaries across site-specific reference dates with `window_pane()`.
5. Screen candidate predictors with `screen_window_features()`.
6. Reduce redundant predictors with `screen_feature_correlations()`.
7. Build FDA-based summaries with interval-driven feature extraction.

## Installation

Install the development version from GitHub:

```r
pak::pak("AlvesKS/windcut")
```

## Example

```r
library(windcut)

weather <- example_weather_data()
windows <- make_windows(min_offset = -20, max_offset = -1, width = 5, reference_col = "assessment_time")

assessments <- data.frame(
  assessment_id = c("A1", "A2"),
  assessment_time = as.POSIXct(c("2024-01-20", "2024-01-25"), tz = "UTC"),
  disease_intensity = c(12.5, 18.0)
)

features <- window_pane(
  weather = weather,
  assessments = assessments,
  id_col = "assessment_id",
  response_col = "disease_intensity",
  windows = windows
)

ranked <- screen_window_features(
  data = features,
  response_col = "disease_intensity",
  method = "spearman"
)

less_redundant <- screen_feature_correlations(
  data = features,
  exclude_cols = c("site_id", "assessment_time", "disease_intensity"),
  method = "spearman",
  threshold = 0.8
)
```

For richer simulated data with daily seasonality, rain events, and one
site-specific disease assessment per weather series, use:

```r
weather <- simulate_weather_series(
  days = 45,
  n_series = 8,
  id_col = "site_id",
  seed = 40
)
assessments <- simulate_assessment_data(weather, id_col = "site_id", seed = 42)
```

The result is a single row per assessment with one column per
metric-window combination, such as:

- `temp_mean_window_m03_z00`
- `rain_sum_window_m05_z00`
- `rh_days_ge_90_window_m10_m05`

## Current metrics

- mean, minimum, and maximum temperature
- cumulative rainfall
- mean relative humidity
- hours above a relative humidity threshold
- cumulative leaf wetness

## Current selection functions

- correlation-based window screening
- Spearman or Pearson association estimates
- multiple-testing correction with `p.adjust()`
- pairwise feature-correlation screening for less redundant modeling inputs

## Current FDA functions

- long-to-matrix conversion for common functional grids
- grouped functional mean summaries and difference curves
- functions for function-on-scalar regression and interval-wise testing
- interval-based feature extraction after FDA screening

## Roadmap

- infection risk indices
- aggregation by hourly or daily resolution
- window selection functions for modeling workflows




