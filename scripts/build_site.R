pandoc_candidates <- c(
  "C:/Users/kai-q/AppData/Local/Programs/Quarto/bin/tools",
  "C:/Program Files/RStudio/resources/app/bin/quarto/bin/tools"
)

local_lib <- file.path(getwd(), ".r-library", format(Sys.time(), "%Y%m%d%H%M%S"))
dir.create(local_lib, recursive = TRUE, showWarnings = FALSE)
.libPaths(c(local_lib, .libPaths()))

pandoc_dir <- pandoc_candidates[file.exists(file.path(pandoc_candidates, "pandoc.exe"))][1]
if (!is.na(pandoc_dir)) {
  Sys.setenv(RSTUDIO_PANDOC = pandoc_dir)
}

cache_dir <- file.path(getwd(), ".cache", "R")
dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
Sys.setenv(R_USER_CACHE_DIR = cache_dir)

deps_src <- file.path(getwd(), "docs", "deps")
offline_deps_root <- file.path(getwd(), ".pkgdown-assets")
dir.create(offline_deps_root, recursive = TRUE, showWarnings = FALSE)
original_cached_dependency <- getFromNamespace("cached_dependency", "pkgdown")

offline_cached_dependency <- function(name, version, files) {
  source_dir <- file.path(deps_src, paste0(name, "-", version))
  local_dir <- file.path(offline_deps_root, paste0(name, "-", version))

  if (dir.exists(source_dir) && !dir.exists(local_dir)) {
    dir.create(local_dir, recursive = TRUE, showWarnings = FALSE)
    file.copy(list.files(source_dir, full.names = TRUE), local_dir, recursive = TRUE, overwrite = TRUE)
  }

  if (dir.exists(local_dir)) {
    dep_files <- list.files(local_dir)
    return(htmltools::htmlDependency(
      name = name,
      version = version,
      src = local_dir,
      script = dep_files[tools::file_ext(dep_files) == "js"],
      stylesheet = dep_files[tools::file_ext(dep_files) == "css"]
    ))
  }

  original_cached_dependency(name, version, files)
}

assignInNamespace("cached_dependency", offline_cached_dependency, ns = "pkgdown")
assignInNamespace("cran_link", function(pkg) NULL, ns = "pkgdown")
assignInNamespace(
  "downlit_html_node",
  function(x, classes = downlit::classes_pandoc()) x,
  ns = "downlit"
)

install_status <- system2(
  file.path(R.home("bin"), "R"),
  c("CMD", "INSTALL", "--no-lock", "-l", shQuote(normalizePath(local_lib, winslash = "/")), ".")
)

if (!identical(install_status, 0L)) {
  stop("Local package installation failed during site build.", call. = FALSE)
}

pkg <- pkgdown::as_pkgdown(".")
pkgdown::init_site(pkg)
pkgdown::build_home(pkg, preview = FALSE)
pkgdown::build_reference(pkg, preview = FALSE)
pkgdown::build_news(pkg, preview = FALSE)

article_dir <- file.path("docs", "articles")
if (dir.exists(article_dir)) {
  unlink(article_dir, recursive = TRUE, force = TRUE)
}
pkgdown:::build_articles_index(pkg)
article_names <- tools::file_path_sans_ext(basename(list.files("vignettes", pattern = "\\.Rmd$")))
asset_probe <- file.path("vignettes", "--find-assets.html")
for (article_name in article_names) {
  unlink(asset_probe, force = TRUE)
  pkgdown::build_article(article_name, pkg = pkg, new_process = FALSE, quiet = TRUE)
  unlink(asset_probe, force = TRUE)
}

asset_dir <- file.path("docs", "assets")
dir.create(asset_dir, recursive = TRUE, showWarnings = FALSE)

png(file.path(asset_dir, "home-window-pane.png"), width = 1200, height = 800, res = 150)
windows <- windcut::make_windows(min_offset = -21, max_offset = -1, width = 5)
plot(
  NA,
  xlim = range(c(0, windows$relative_start, windows$relative_end)),
  ylim = c(1, min(18, nrow(windows))),
  xlab = "Time relative to assessment (days)",
  ylab = "Window candidate",
  main = "Window-pane candidate grid",
  yaxt = "n"
)
cols <- colorRampPalette(c("#dcebdc", "#3f7d58"))(min(18, nrow(windows)))
for (i in seq_len(min(18, nrow(windows)))) {
  segments(windows$relative_start[i], i, windows$relative_end[i], i, lwd = 5, col = cols[i])
}
axis(2, at = seq_len(min(18, nrow(windows))), labels = windows$label[seq_len(min(18, nrow(windows)))], las = 1, cex.axis = 0.7)
box(col = "#b7cdbd")
dev.off()

png(file.path(asset_dir, "home-functional-curves.png"), width = 1200, height = 800, res = 150)
set.seed(2301)
site_profiles <- data.frame(
  site_index = 1:40,
  rh_shift = seq(-18, 22, length.out = 40) + rnorm(40, mean = 0, sd = 0.8),
  rain_shift = seq(-2, 5.5, length.out = 40) + rnorm(40, mean = 0, sd = 0.2),
  leaf_wetness_shift = seq(-0.35, 0.55, length.out = 40) + rnorm(40, mean = 0, sd = 0.02),
  temp_shift = seq(-1.2, 2, length.out = 40) + rnorm(40, mean = 0, sd = 0.1)
)

build_demo_site <- function(i, site_profile) {
  hourly_weather <- windcut::simulate_weather_series(days = 81, seed = 500 + i)
  hourly_weather <- dplyr::mutate(
    hourly_weather,
    site_id = sprintf("S%02d", i),
    dap = as.integer(difftime(as.Date(time), as.Date(min(time)), units = "days")) - 30
  )

  late_window <- hourly_weather$dap >= 20 & hourly_weather$dap <= 50

  hourly_weather <- dplyr::mutate(
    hourly_weather,
    rh = dplyr::if_else(late_window, pmin(100, pmax(35, rh + site_profile$rh_shift)), rh),
    rain = dplyr::if_else(late_window, pmax(0, rain + site_profile$rain_shift), rain),
    leaf_wetness = dplyr::if_else(
      late_window,
      pmin(1, pmax(0, leaf_wetness + site_profile$leaf_wetness_shift)),
      leaf_wetness
    ),
    temp = dplyr::if_else(late_window, temp + site_profile$temp_shift, temp)
  )

  daily_weather <- dplyr::group_by(
    dplyr::mutate(hourly_weather, day = as.Date(time)),
    site_id,
    dap,
    day
  ) |>
    dplyr::summarise(
      temp = mean(temp),
      rh = mean(rh),
      rain = sum(rain),
      leaf_wetness = mean(leaf_wetness),
      .groups = "drop"
    )

  list(hourly = hourly_weather, daily = daily_weather)
}

demo_sites <- lapply(1:40, function(i) {
  build_demo_site(
    i,
    site_profile = site_profiles[site_profiles$site_index == i, , drop = FALSE]
  )
})

weather_hourly <- dplyr::bind_rows(lapply(demo_sites, `[[`, "hourly"))
weather_daily <- dplyr::bind_rows(lapply(demo_sites, `[[`, "daily"))

assessments <- windcut::simulate_assessment_data(
  weather = weather_hourly,
  id_col = "site_id",
  response_type = "binary",
  seed = 23,
  response_col = "wm"
)

subject_data <- dplyr::left_join(
  weather_daily,
  data.frame(
    site_id = assessments$site_id,
    wm_class = factor(assessments$wm, levels = c(0, 1), labels = c("wm-", "wm+"))
  ),
  by = "site_id"
)

curve_data <- windcut::functional_group_summary(
  data = subject_data,
  id_col = "site_id",
  time_col = "dap",
  value_col = "rh",
  group_col = "wm_class",
  method = "spline",
  smooth_args = list(spar = 0.65)
)$summary

plot_data <- dplyr::mutate(
  curve_data,
  group = factor(group, levels = c("wm-", "wm+"))
)

plot_obj <- ggplot2::ggplot(
  plot_data,
  ggplot2::aes(x = time, y = mean_value, color = group)
) +
  ggplot2::annotate(
    "rect",
    xmin = 20,
    xmax = 50,
    ymin = -Inf,
    ymax = Inf,
    fill = "#c6d8ea",
    alpha = 0.45
  ) +
  ggplot2::geom_line(linewidth = 1.8) +
  ggplot2::scale_color_manual(values = c("wm-" = "#2b6c4f", "wm+" = "#c47f2c")) +
  ggplot2::labs(
    title = "Functional weather contrast",
    subtitle = "Smoothed daily relative humidity means for wm- and wm+ sites",
    x = "Days after planting",
    y = "Mean relative humidity (%)",
    color = NULL
  ) +
  ggplot2::theme_minimal(base_size = 16) +
  ggplot2::theme(
    legend.position = "inside",
    legend.position.inside = c(0.12, 0.9),
    panel.grid.minor = ggplot2::element_blank(),
    plot.title = ggplot2::element_text(face = "bold"),
    plot.title.position = "plot"
  )

print(plot_obj)
dev.off()



