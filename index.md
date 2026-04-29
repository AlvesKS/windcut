---
title: windcut
---

# windcut <img src="logo.png" align="right" height="180" alt="windcut logo" class="home-inline-logo" />

`windcut` helps plant disease epidemiologists convert weather time series into
relative-time window features that can be used in forecasting and explanatory
models of disease intensity.

::: {.grid-cards}
::: {.feature-card}
### Slice

Generate many candidate windows relative to a user-chosen reference date using
a window-pane strategy. Use fixed-width windows for a classic sliding scan, or
variable-width windows when the exposure duration is uncertain.
:::

::: {.feature-card}
### Summarize

Turn windows into biologically interpretable predictors such as wetness,
rainfall, temperature, and humidity summaries.
:::

::: {.feature-card}
### Screen

Rank candidate predictors with correlation-based screening and multiple-testing
correction, then reduce highly correlated predictors before downstream modeling.
:::
:::

## Learning path

1. Start with the Getting Started tutorial to understand the core workflow.
2. Move to the window-pane tutorial to think like an epidemiologist when defining candidate periods.
3. Use the feature-screening tutorial to prioritize windows before fitting predictive models.
4. Explore the FDA tutorial when you want to work with entire weather trajectories instead of pre-cut windows.

## Quick start

The quick start uses the bundled demo dataset. It loads daily weather and one
disease assessment per site, defines sliding weather windows, builds
model-ready predictors, ranks features against the response, and identifies a
less redundant predictor set for modeling.

```r
library(windcut)

data(window_pane_demo_data)

weather <- window_pane_demo_data$weather
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
  windows = windows,
  id_col = "site_id",
  response_col = "disease_intensity",
  statistics = list(
    daily_mean_temp = list(mean = "mean", days_18_26 = count_between(18, 26)),
    daily_mean_rh = list(mean = "mean", humid_days = humid_hours(90)),
    daily_sum_rain = list(total = "sum")
  )
)

screened <- screen_window_features(
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

## Core ideas

- Window-pane analysis is useful when the biologically relevant period is not known in advance.
- Weather summaries should stay interpretable enough to discuss with domain experts.
- Highly correlated predictors can be screened before modeling workflows that need less redundant inputs.
- Screening is only one step; the best windows still need validation in predictive models.

## Visual Overview

### Candidate windows

![Window pane candidate grid](assets/home-window-pane.png)

### Functional weather contrast

![Functional weather curves](assets/home-functional-curves.png)




