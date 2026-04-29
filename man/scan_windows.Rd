\name{scan_windows}
\alias{scan_windows}
\title{Scan Candidate Weather Windows Relative to a Reference Time}
\usage{
scan_windows(
  weather,
  reference_time,
  windows,
  weather_cols = NULL,
  time_col = "time",
  temp_col = "temp",
  rh_col = "rh",
  rain_col = "rain",
  leaf_wetness_col = "leaf_wetness",
  unit = c("days", "hours"),
  statistics = list(
    temp = c("mean", "min", "max"),
    rh = "mean",
    rain = "sum",
    leaf_wetness = "sum"
  )
)
}
\arguments{
  \item{weather}{A weather data frame.}

  \item{reference_time}{Reference time used to place the candidate windows.}

  \item{windows}{A data frame created by \code{\link{make_windows}}.}

  \item{weather_cols}{Character vector of weather columns to summarize. If
named, the names are used in the output feature names.}

  \item{time_col}{Name of the weather timestamp column.}

  \item{temp_col}{Name of the temperature column.}

  \item{rh_col}{Name of the relative humidity column.}

  \item{rain_col}{Name of the rainfall column.}

  \item{leaf_wetness_col}{Name of the leaf wetness column.}

  \item{unit}{Time unit for relative-time windows. One of \code{"days"} or
\code{"hours"}.}

  \item{statistics}{Summary statistics passed to \code{\link[=summarise_weather_window]{summarise_weather_window()}}.}
}
\value{
A data frame with one row per candidate window and summary metrics.
}
\description{
For each candidate window, subsets the weather series relative to a reference
time and computes summary features. The reference can be an assessment date,
planting date, flowering date, inoculation date, or any other biologically
meaningful timestamp.
}




