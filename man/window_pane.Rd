\name{window_pane}
\alias{window_pane}
\title{Build a Wide Feature Table from Weather Windows}
\usage{
window_pane(
  weather,
  assessments,
  windows,
  reference_col = NULL,
  id_col = NULL,
  response_col = NULL,
  weather_cols = NULL,
  time_col = "time",
  temp_col = "temp",
  rh_col = "rh",
  rain_col = "rain",
  leaf_wetness_col = "leaf_wetness",
  unit = c("days", "hours"),
  rh_threshold = 90,
  statistics = list(temp = c("mean", "min", "max"), rh = "mean", rain = "sum", leaf_wetness =
    "sum")
)
}
\arguments{
  \item{weather}{A weather data frame.}

\item{assessments}{A data frame containing reference times and optional IDs
or responses.}

  \item{windows}{A data frame created by \code{\link{make_windows}}.}

\item{reference_col}{Name of the reference timestamp column. If \code{NULL},
the function uses metadata stored by \code{\link{make_windows}} or falls back to
\code{"assessment_time"}.}

\item{id_col}{Optional site, location, or assessment identifier column. When
this column is present in both \code{weather} and \code{assessments}, each row is
matched to the weather series from the same ID.}

  \item{response_col}{Optional response column, such as disease intensity.}

  \item{weather_cols}{Character vector of weather columns to summarize. If
named, the names are used in the output feature names.}

  \item{time_col}{Name of the weather timestamp column.}

  \item{temp_col}{Name of the temperature column.}

  \item{rh_col}{Name of the relative humidity column.}

  \item{rain_col}{Name of the rainfall column.}

  \item{leaf_wetness_col}{Name of the leaf wetness column.}

  \item{unit}{Time unit for relative-time windows. One of \code{"days"} or
\code{"hours"}.}

\item{rh_threshold}{Optional threshold value retained in the API. Threshold
counts are computed through \code{statistics}, for example
\code{list(rh = list(days_ge_90 = count_ge(90)))}.}

  \item{statistics}{Summary statistics passed to \code{\link[=summarise_weather_window]{summarise_weather_window()}}.}
}
\value{
A wide data frame with one row per reference event.
}
\description{
Applies \code{\link{scan_windows}} to many site-specific reference dates and returns a
wide table with one row per reference event and one column per metric-window
combination. The reference date can be an assessment date, planting date, or
any other timestamp stored in the input table.
}




