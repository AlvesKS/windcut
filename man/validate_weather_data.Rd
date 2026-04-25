\name{validate_weather_data}
\alias{validate_weather_data}
\title{Validate a Weather Time Series}
\usage{
validate_weather_data(
  weather,
  time_col = "time",
  required_cols = c("temp", "rh", "rain", "leaf_wetness")
)
}
\arguments{
  \item{weather}{A data frame with weather observations.}

  \item{time_col}{Name of the timestamp column.}

  \item{required_cols}{Character vector of required weather columns.}
}
\value{
A cleaned data frame sorted by time.
}
\description{
Checks that a weather table contains the expected columns, converts the time
column to \code{POSIXct}, sorts observations by time, and removes duplicated
timestamps.
}




