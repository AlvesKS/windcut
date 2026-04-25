pkgload::load_all(".")

dir.create("data", showWarnings = FALSE)

window_weather <- simulate_weather_series(
  start = as.POSIXct("2023-12-01 00:00:00", tz = "UTC"),
  days = 180,
  n_series = 10,
  id_col = "site_id",
  seed = 70
)

window_weather_daily <- aggregate_weather_daily(
  weather = window_weather,
  id_col = "site_id",
  statistics = list(
    temp = "mean",
    rh = "mean",
    rain = "sum",
    leaf_wetness = "sum"
  )
)

window_assessments <- simulate_assessment_data(
  weather = window_weather,
  id_col = "site_id",
  seed = 7
)

set.seed(704)
n_window_sites <- nrow(window_assessments)
planting_dates <- as.Date("2024-01-10") + sample(0:41, n_window_sites, replace = FALSE)
assessment_offsets <- 90 + sample(-5:5, n_window_sites, replace = TRUE)

window_assessments <- dplyr::mutate(
  window_assessments,
  planting_time = as.POSIXct(planting_dates, tz = "UTC"),
  assessment_time = planting_time + assessment_offsets * 86400
)

window_pane_demo_data <- list(
  weather = window_weather_daily,
  weather_hourly = window_weather,
  assessments = window_assessments
)

set.seed(2301)

site_profiles <- tibble::tibble(
  site_index = 1:80,
  rh_shift = seq(-20, 25, length.out = 80) + stats::rnorm(80, mean = 0, sd = 1),
  rain_shift = seq(-2.5, 6.5, length.out = 80) + stats::rnorm(80, mean = 0, sd = 0.25),
  leaf_wetness_shift = seq(-0.45, 0.65, length.out = 80) + stats::rnorm(80, mean = 0, sd = 0.02),
  temp_shift = seq(-1.5, 2.5, length.out = 80) + stats::rnorm(80, mean = 0, sd = 0.1)
)

build_demo_site <- function(i, site_profile) {
  hourly_weather <- simulate_weather_series(days = 81, seed = 500 + i)
  daily_weather <- hourly_weather %>%
    dplyr::mutate(day = as.Date(time)) %>%
    dplyr::group_by(day) %>%
    dplyr::summarise(
      temp = mean(temp),
      rh = mean(rh),
      rain = sum(rain),
      leaf_wetness = mean(leaf_wetness),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      time = as.POSIXct(day, tz = "UTC"),
      dap = -30:50,
      site_id = sprintf("S%02d", i)
    ) %>%
    dplyr::select(time, temp, rh, rain, leaf_wetness, dap, site_id)

  late_window <- daily_weather$dap >= 20 & daily_weather$dap <= 50

  dplyr::mutate(
    daily_weather,
    rh = dplyr::if_else(late_window, pmin(100, pmax(35, rh + site_profile$rh_shift)), rh),
    rain = dplyr::if_else(late_window, pmax(0, rain + site_profile$rain_shift), rain),
    leaf_wetness = dplyr::if_else(
      late_window,
      pmin(1, pmax(0, leaf_wetness + site_profile$leaf_wetness_shift)),
      leaf_wetness
    ),
    temp = dplyr::if_else(late_window, temp + site_profile$temp_shift, temp)
  )
}

fda_weather_daily <- dplyr::bind_rows(lapply(1:80, function(i) {
  build_demo_site(
    i,
    site_profile = site_profiles %>% dplyr::filter(site_index == i)
  )
}))

fda_assessments <- simulate_assessment_data(
  weather = fda_weather_daily,
  id_col = "site_id",
  response_type = "binary",
  seed = 23,
  response_col = "wm"
)

fda_assessments <- dplyr::mutate(
  fda_assessments,
  wm_class = factor(wm, levels = c(0, 1), labels = c("wm-", "wm+"))
)

fda_demo_data <- list(
  weather_daily = fda_weather_daily,
  assessments = fda_assessments,
  variable_specs = data.frame(
    variable = c("temp", "rh", "rain", "leaf_wetness"),
    label = c(
      "Mean air temperature (deg C)",
      "Relative humidity (%)",
      "Rainfall (mm day-1)",
      "Leaf wetness proportion"
    ),
    stringsAsFactors = FALSE
  )
)

save(window_pane_demo_data, file = "data/window_pane_demo_data.rda", compress = "xz")
save(fda_demo_data, file = "data/fda_demo_data.rda", compress = "xz")
