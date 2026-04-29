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

weather <- window_pane_demo_data$weather_daily
assessments <- window_pane_demo_data$assessments

windows <- make_windows(
  min_offset = -21,
  max_offset = 0,
  width = 7,
  reference_col = "assessment_time"
)

features <- window_pane(
  weather = weather,
  assessments = assessments,
  id_col = "site_id",
  response_col = "disease_intensity",
  windows = windows,
  reference_col = "assessment_time",
  weather_cols = c(
    "daily_mean_temp",
    "daily_mean_rh",
    "daily_sum_rain",
    "daily_sum_leaf_wetness"
  ),
  statistics = list(
    daily_mean_temp = c("mean", "max"),
    daily_mean_rh = list(humid_days = count_at_or_above(90)),
    daily_sum_rain = list(total = "sum"),
    daily_sum_leaf_wetness = list(wet_days = count_above(0))
  )
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

## Biologically meaningful weather summaries

`statistics` can use ordinary R summary names, custom functions, and
multivariable conditions. This lets you create predictors such as humid
observations, temperature-range exposure, rain events, wetness spells, and
infection-favorable periods.

```r
weather <- simulate_weather_series()

weather <- weather |>
  derive_vpd(temp, rh) |>
  derive_leaf_wetness_from_rh(rh, threshold = 90)

summary_statistics <- list(
  temp = list(
    mean = "mean",
    max = "max",
    hours_18_26 = count_between(18, 26),
    degree_hours_10 = degree_hours_above(10)
  ),
  rh = list(
    mean = "mean",
    humid_hours = humid_hours(90),
    prop_humid = proportion_at_or_above(90)
  ),
  rain = list(
    total = "sum",
    rainy_hours = rainy_hours(0),
    rain_events = rain_events(0.2)
  ),
  leaf_wetness_est = list(
    wet_hours = wet_hours(0),
    max_wet_spell = max_consecutive_wet_hours(0)
  ),
  .conditions = list(
    favorable_hours = count_when(temp >= 18 & temp <= 26 & rh >= 90),
    max_favorable_spell = max_consecutive_when(temp >= 18 & temp <= 26 & rh >= 90),
    warm_rain_total = sum_when(rain, temp >= 18 & temp <= 28)
  )
)

windows <- make_windows(min_offset = -72, max_offset = 0, width = 24, reference_col = "assessment_time")
assessments <- simulate_assessment_data(weather)

features <- window_pane(
  weather = weather,
  assessments = assessments,
  windows = windows,
  reference_col = "assessment_time",
  statistics = summary_statistics
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
- `rh_days_at_or_above_90_window_m10_m05`

## Current summaries

- threshold counts and proportions
- conditional sums and means
- thermal accumulation
- rain-event summaries
- wetness and humidity spell summaries
- multivariable disease-favorable conditions

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
- window selection functions for modeling workflows




