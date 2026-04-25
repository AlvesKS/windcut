\name{summarise_weather_window}
\alias{summarise_weather_window}
\title{Summarise Weather Inside a Single Window}
\usage{
summarise_weather_window(
  weather,
  weather_cols = NULL,
  temp_col = "temp",
  rh_col = "rh",
  rain_col = "rain",
  leaf_wetness_col = "leaf_wetness",
  rh_threshold = 90,
  statistics = list(temp = c("mean", "min", "max"), rh = "mean", rain = "sum", leaf_wetness =
    "sum")
)
}
\arguments{
  \item{weather}{A validated weather data frame.}

  \item{weather_cols}{Character vector of weather columns to summarize. If
named, the names are used in the output feature names.}

  \item{temp_col}{Name of the temperature column.}

  \item{rh_col}{Name of the relative humidity column.}

  \item{rain_col}{Name of the rainfall column.}

  \item{leaf_wetness_col}{Name of the leaf wetness column.}

\item{rh_threshold}{Optional threshold value retained in the API. Threshold
counts are computed through \code{statistics}, for example
\code{list(rh = list(days_ge_90 = count_ge(90)))}.}

  \item{statistics}{Summary statistics to compute. Use a character vector to
apply the same statistics to all \code{weather_cols}, such as
\code{c("mean", "sd", "IQR")}. Use a named list to choose statistics by
variable or to provide custom named functions.}
}
\value{
A one-row data frame with summary metrics.
}
\description{
Computes a small set of epidemiology-friendly summaries from a subset of a
weather time series.
}




